use std::{io, fmt, collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::{
    token::{Token, TokenType},
    parser::{Ast, Expression::*, Statement::*, StmtIndex, ExprIndex},
    resolver::ReferenceTable,
};

pub struct Interpreter<'io> {
    globals: HashMap<String, Value>,
    environments: Vec<Environment>,
    stack: Option<EnvIndex>,
    stdout: &'io mut dyn io::Write,
    native_functions: Vec<NativeFn>,
    objects: Vec<HashMap<String, Value>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Class(Rc<Class>),
    Function {
        parameters: Vec<String>,
        body: Vec<StmtIndex>,
        environment: Option<EnvIndex>
    },
    NativeFunction(NativeFnIndex),
    Nil,
    Number(f64),
    ObjectRef {
        class: Rc<Class>,
        instance: ObjectId,
    },
    String(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EnvIndex(usize);

pub type NativeFn = fn(&Vec<Value>) -> Result<Value, NativeError>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeFnIndex(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ObjectId(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    name: String,
    methods: HashMap<String, Value>,
}

#[derive(Clone, Debug, Error)]
pub enum RuntimeError<'s> {
    #[error("(line {}): Arity mismatch, expected {} parameters, found {}",
            location.line, expected, provided)]
    ArityMismatch {
        expected: usize,
        provided: usize,
        location: Token<'s>
    },

    #[error("(line {}): Binary '{operator}' is not appicable to {lhs} \
             and {rhs}", operator.line)]
    BinaryOperatorNotApplicable {
        lhs: Value,
        rhs: Value,
        operator: Token<'s>
    },

    #[error("Unhandled stack unwind")]
    StackUnwind(Option<Value>),

    #[error("(line {}): Type mismatch, expected {}, found {} '{}'",
            location.line, expected, provided.value_type(), provided)]
    TypeMismatch {
        expected: &'static str,
        provided: Value,
        location: Token<'s>
    },

    #[error("(line {}): Unary '{operator}' is not applicable to {value}",
            operator.line)]
    UnaryOperatorNotApplicable {
        value: Value,
        operator: Token<'s>
    },

    #[error("(line {}): Unresolved identifier '{0}'", .0.line)]
    UnresolvedIdentifier(Token<'s>),
}

#[derive(Clone, Debug)]
pub enum NativeError {
    ArityMismatch(usize),
    Failed(String),
    TypeMismatch(&'static str, Value),
}

struct Environment {
    locals: HashMap<String, Value>,
    parent: Option<EnvIndex>,
}

type EvalResult<'s> = Result<Option<Value>, RuntimeError<'s>>;
type ExprResult<'s> = Result<Value, RuntimeError<'s>>;

impl<'io> Interpreter<'io> {
    pub fn new(stdout: &'io mut dyn io::Write) -> Interpreter<'io> {
        Interpreter {
            globals: HashMap::new(),
            environments: Vec::new(),
            stack: None,
            stdout,
            native_functions: Vec::new(),
            objects: Vec::new(),
        }
    }

    pub fn expose<T: AsRef<str>>(&mut self, name: T, function: NativeFn) {
        let index = NativeFnIndex(self.native_functions.len());
        self.native_functions.push(function);
        self.globals.insert(
            name.as_ref().to_owned(),
            Value::NativeFunction(index)
        );
    }

    pub fn evaluate<'s>(
        &mut self, ast: &Ast<'s>, refs: &ReferenceTable
    ) -> EvalResult<'s> {
        let mut last_value = None;
        for statement in ast.top_level_statements() {
            last_value = self.eval_statement(*statement, ast, refs)?;
        }

         Ok(last_value)
    }

    fn define(&mut self, identifier: &str, value: Value) {
        if let Some(index) = self.stack {
            let active_scope = &mut self.environments[index.0];
            let _ = active_scope.locals.insert(identifier.to_owned(), value);
        } else {
            let _ = self.globals.insert(identifier.to_owned(), value);
        }
    }

    fn resolve(&self, identifier: &str, depth: Option<&usize>) -> Option<Value> {
        const SMALL_STACK: &str = "Stack smaller than ReferenceTable expected";

        if let Some(hops_count) = depth {
            let stack_level = self.stack.expect(SMALL_STACK);
            let mut stack_frame = &self.environments[stack_level.0];

            for _ in 0..*hops_count {
                let parent_level = stack_frame.parent.expect(SMALL_STACK);
                stack_frame = &self.environments[parent_level.0];
            }

            if let Some(value) = stack_frame.locals.get(identifier) {
                Some(value.clone())
            } else {
                None
            }
        } else {
            self.globals.get(identifier).cloned()
        }
    }

    fn assign(&mut self, identifier: &str, new_value: Value) -> bool {
        let mut stack_frame = self.stack;
        while let Some(index) = stack_frame {
            let environment = &mut self.environments[index.0];
            if let Some(bound_value) = environment.locals.get_mut(identifier) {
                *bound_value = new_value;
                return true
            }
            stack_frame = environment.parent;
        }

        if let Some(bound_value) = self.globals.get_mut(identifier) {
            *bound_value = new_value;
            true
        } else {
            false
        }
    }

    fn push_scope(&mut self) {
        let (env_id, _env) = self.allocate_environment(self.stack);
        self.stack = Some(env_id);
    }

    fn pop_scope(&mut self) -> EnvIndex {
        let index = self.stack.expect("Attempt to pop empty stack");
        let leaving_scope = &self.environments[index.0];
        self.stack = leaving_scope.parent;
        index
    }

    fn allocate_environment(
        &mut self, parent: Option<EnvIndex>
    ) -> (EnvIndex, &mut Environment) {
        let env_id = EnvIndex(self.environments.len());
        self.environments.push(Environment { locals: HashMap::new(), parent });
        (env_id, &mut self.environments[env_id.0])
    }

    fn swap_stack(&mut self, new_stack: Option<EnvIndex>) -> Option<EnvIndex> {
        std::mem::replace(&mut self.stack, new_stack)
    }

    fn call_native_fn(
        &mut self, function: NativeFnIndex, arguments: &Vec<Value>
    ) -> Result<Value, NativeError> {
        let f = self.native_functions[function.0];
        f(arguments)
    }

    // We bind the this keyword to an object by wrapping the methods
    // scope in a new scope that defines "this" as a reference to the object.
    fn bind_this(&mut self, function: &Value, object: &Value) -> Value {
        if let Value::Function { parameters, body, environment } = function {
            let (object_env_id, env) = self.allocate_environment(*environment);
            env.locals.insert("this".to_owned(), object.clone());
            Value::Function {
                parameters: parameters.clone(),
                body: body.clone(),
                environment: Some(object_env_id)
            }
        } else {
            // This is enforced by the parser.
            unreachable!("bind_this() called with non-function value")
        }
    }

    fn eval_statement<'s>(
        &mut self, statement: StmtIndex, ast: &Ast<'s>, refs: &ReferenceTable
    ) -> EvalResult<'s> {
        match ast.statement(statement) {
            Block(statements) => {
                self.push_scope();
                self.eval_block(statements, ast, refs)?;
                let _ = self.pop_scope();

                Ok(None)
            }

            Class(name, method_definitions) => {
                self.define(name.lexeme, Value::Nil);

                let mut methods = HashMap::new();
                for method_definition in method_definitions {
                    let method = ast.statement(*method_definition);
                    if let Fun(method_name, params, body) = method {
                        let parameters = params.iter()
                            .map(|t| t.lexeme.to_owned())
                            .collect();
                        methods.insert(
                            method_name.lexeme.to_owned(),
                            Value::Function {
                                parameters,
                                body: body.clone(),
                                environment: self.stack
                            }
                        );
                    } else {
                        unreachable!("Encountered non-function method")
                    }
                }

                let class = Rc::new(Class {
                    name: name.lexeme.to_owned(),
                    methods,
                });
                self.assign(name.lexeme, Value::Class(class));

                Ok(None)
            }

            Expression(expr) => self.eval_expression(*expr, ast, refs)
                .map(|v| Some(v)),

            Fun(name, params, body) => {
                let parameters = params.iter()
                    .map(|t| t.lexeme.to_owned())
                    .collect();
                self.define(name.lexeme, Value::Function {
                    parameters,
                    body: body.clone(),
                    environment: self.stack
                });

                Ok(None)
            }

            If(condition, then_block, optional_else_block) => {
                if self.eval_expression(*condition, ast, refs)?.is_truthy() {
                    self.eval_statement(*then_block, ast, refs)?;
                } else if let Some(else_block) = optional_else_block {
                    self.eval_statement(*else_block, ast, refs)?;
                }

                Ok(None)
            }

            Print(expr) => {
                match self.eval_expression(*expr, ast, refs)? {
                    Value::String(s) => writeln!(self.stdout, "{}", s),
                    result           => writeln!(self.stdout, "{}", result)
                }.expect("Failed to write to stdout");

                Ok(None)
            }

            Return(_token, optional_expression) => {
                let return_value = if let Some(expr) = optional_expression {
                    Some(self.eval_expression(*expr, ast, refs)?)
                } else {
                    None
                };

                // This is a small abuse of the error handling, but it's the exact
                // unwind semantics that we want and by far the simplest way.
                Err(RuntimeError::StackUnwind(return_value))
            }

            Var(identifier, optional_initialiser) => {
                let value = if let Some(initialiser) = optional_initialiser {
                    self.eval_expression(*initialiser, ast, refs)?
                } else {
                    Value::Nil
                };
                self.define(identifier.lexeme, value);

                Ok(None)
            }

            While(condition, body) => {
                while self.eval_expression(*condition, ast, refs)?.is_truthy() {
                    self.eval_statement(*body, ast, refs)?;
                }

                Ok(None)
            }
        }
    }

    fn eval_expression<'s>(
        &mut self, expression: ExprIndex, ast: &Ast<'s>, refs: &ReferenceTable
    ) -> ExprResult<'s> {
        use Value::*;
        use TokenType as TT;

        match ast.expression(expression) {
            Access(object, name) => {
                let object = self.eval_expression(*object, ast, refs)?;
                if let ObjectRef { ref class, instance } = object {
                    let fields = &self.objects[instance.0];
                    if let Some(value) = fields.get(name.lexeme) {
                        Ok(value.clone())
                    } else if let Some(
                        function @ Function { .. }
                    ) = class.methods.get(name.lexeme) {
                        Ok(self.bind_this(function, &object))
                    } else {
                        Err(RuntimeError::UnresolvedIdentifier(*name))
                    }
                } else {
                    Err(RuntimeError::TypeMismatch {
                        expected: "object",
                        provided: object,
                        location: *name,
                    })
                }
            }

            Assign(variable, expr) => {
                let value = self.eval_expression(*expr, ast, refs)?;
                if self.assign(variable.lexeme, value.clone()) {
                    Ok(value)
                } else {
                    Err(RuntimeError::UnresolvedIdentifier(*variable))
                }
            }

            Binary(lhs, token, rhs) => {
                let left  = self.eval_expression(*lhs, ast, refs)?;
                let right = self.eval_expression(*rhs, ast, refs)?;

                match token.token_type {
                    TT::Greater      => greater(&left, &right, token),
                    TT::GreaterEqual => greater_eq(&left, &right, token),
                    TT::Less         => less(&left, &right, token),
                    TT::LessEqual    => less_eq(&left, &right, token),
                    TT::EqualEqual   => Ok(Boolean(left == right)),
                    TT::BangEqual    => Ok(Boolean(left != right)),
                    TT::Minus        => minus(&left, &right, token),
                    TT::Slash        => divide(&left, &right, token),
                    TT::Star         => multiply(&left, &right, token),
                    TT::Plus         => plus(&left, &right, token),
                    _ => unreachable!("Binary operator other than (+|-|*|/)")
                }
            }

            Call(callee, token, args) => {
                match self.eval_expression(*callee, ast, refs)? {
                    function @ Function { .. } => {
                        self.eval_fn(token, &function, args, ast, refs)
                    },

                    NativeFunction(function) => {
                        let arguments = args.iter()
                            .map(|arg| self.eval_expression(*arg, ast, refs))
                            .collect::<Result<Vec<Value>, _>>()?;

                        self.call_native_fn(function, &arguments).map_err(|e| {
                            match e {
                                NativeError::ArityMismatch(expected_arity) => {
                                    RuntimeError::ArityMismatch {
                                        expected: expected_arity,
                                        provided: arguments.len(),
                                        location: *token
                                    }
                                }
                                NativeError::TypeMismatch(expected, provided) => {
                                    RuntimeError::TypeMismatch {
                                        expected,
                                        provided,
                                        location: *token
                                    }
                                }
                                _ => panic!("Native call returned error")
                            }
                        })
                    },

                    Class(class) => {
                        // Allocate a new object
                        let instance = ObjectId(self.objects.len());
                        self.objects.push(HashMap::new());
                        let object = Value::ObjectRef {
                            class: class.clone(),
                            instance
                        };

                        if let Some(constructor) = class.methods.get("init") {
                            let method = self.bind_this(constructor, &object);
                            self.eval_fn(token, &method, args, ast, refs)?;
                        }

                        Ok(object)
                    },

                    value => return Err(RuntimeError::TypeMismatch {
                        expected: "callable type",
                        provided: value,
                        location: *token
                    })
                }
            }

            Grouping(expr) => self.eval_expression(*expr, ast, refs),

            Literal(token_type) => match *token_type {
                TT::Number(n)  => Ok(Number(n)),
                TT::String(s)  => Ok(String(s.to_owned())),
                TT::True       => Ok(Boolean(true)),
                TT::False      => Ok(Boolean(false)),
                TT::Nil        => Ok(Nil),
                _ => unreachable!(
                    "Literal other than (number | string | true | false | nil)"
                )
            }

            Logical(lhs, token, rhs) => {
                let left = self.eval_expression(*lhs, ast, refs)?;

                match token.token_type {
                    TT::And => if left.is_falsey() { return Ok(left) },
                    TT::Or  => if left.is_truthy() { return Ok(left) },
                    _ => unreachable!("Logical operator other than (and | or)")
                }

                self.eval_expression(*rhs, ast, refs)
            }

            Mutate(object, name, value) => {
                let object = self.eval_expression(*object, ast, refs)?;
                if let ObjectRef { instance, .. } = object {
                    let new_value = self.eval_expression(*value, ast, refs)?;
                    let fields = &mut self.objects[instance.0];
                    fields.insert(name.lexeme.to_owned(), new_value.clone());
                    Ok(new_value)
                } else {
                    Err(RuntimeError::TypeMismatch {
                        expected: "object",
                        provided: object,
                        location: *name,
                    })
                }
            }

            SelfRef(keyword) => Ok(self.resolve(
                keyword.lexeme, refs.get(&expression)
            ).expect("Unbound 'this' in object scope")),

            Unary(token, rhs) => {
                let value = self.eval_expression(*rhs, ast, refs)?;

                match token.token_type {
                    TT::Minus => negate(&value, token),
                    TT::Bang  => Ok(Boolean(!value.is_truthy())),
                    _ => unreachable!("Unary operation other than (!|-)")
                }
            }

            Variable(identifier) => self
                .resolve(identifier.lexeme, refs.get(&expression))
                .ok_or(RuntimeError::UnresolvedIdentifier(*identifier)),
        }
    }

    fn eval_fn<'s>(
        &mut self,
        token: &Token<'s>,
        function: &Value,
        args: &Vec<ExprIndex>,
        ast: &Ast<'s>,
        refs: &ReferenceTable
    ) -> ExprResult<'s> {
        if let Value::Function { parameters, body, environment } = function {
            if args.len() != parameters.len() {
                return Err(RuntimeError::ArityMismatch {
                    expected: parameters.len(),
                    provided: args.len(),
                    location: *token
                })
            }

            let stack = self.swap_stack(*environment);
            self.push_scope();

            let arg_param_pairs = args.iter().zip(parameters.iter());
            for (arg, parameter) in arg_param_pairs {
                let argument = self.eval_expression(*arg, ast, refs)?;
                self.define(parameter, argument);
            }
            let return_value = self.eval_block(&body, ast, refs);

            self.pop_scope();
            self.swap_stack(stack);

            match return_value {
                Ok(value)
                    => Ok(value.unwrap_or(Value::Nil)),
                Err(RuntimeError::StackUnwind(value))
                    => Ok(value.unwrap_or(Value::Nil)),
                Err(a_real_error)
                    => Err(a_real_error),
            }
        } else {
            panic!("Called eval_fn on non-function value")
        }
    }

    fn eval_block<'s>(
        &mut self, body: &Vec<StmtIndex>, ast: &Ast<'s>, refs: &ReferenceTable
    ) -> EvalResult<'s> {
        let mut last_value = None;
        for statement in body {
            last_value = self.eval_statement(*statement, ast, refs)?
        }

        Ok(last_value)
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Boolean(false) => false,
            _                                  => true
        }
    }

    fn is_falsey(&self) -> bool {
        ! self.is_truthy()
    }

    pub fn value_type(&self) -> &'static str {
        use Value::*;

        match self {
            Boolean(_)        => "boolean",
            Class(_)          => "class",
            Function { .. }   => "function",
            NativeFunction(_) => "native function",
            Nil               => "nil",
            Number(_)         => "number",
            ObjectRef { .. }  => "object",
            String(_)         => "string",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Boolean(b)              => write!(f, "{}", b),
            Class(class)            => write!(f, "<class {}>", &class.name),
            Function { .. }         => write!(f, "<fn>"),
            NativeFunction(_)       => write!(f, "<native fn>"),
            Nil                     => write!(f, "nil"),
            Number(n)               => write!(f, "{}", n),
            ObjectRef { class, .. } => write!(f, "<object {}>", &class.name),
            String(s)               => write!(f, "\"{}\"", s,)
        }
    }
}

macro_rules! binary_operator (
    ($name:ident, $op:tt, $($type:tt)|+ -> $ret:tt) => {
        fn $name<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> ExprResult<'s> {
            use Value::*;

            match (lhs, rhs) {
                $( ($type(l), $type(r)) => Ok($ret(l $op r)), )*

                _ => Err(RuntimeError::BinaryOperatorNotApplicable {
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    operator: *operator
                })
            }
        }
    }
);

binary_operator!(greater,    >,  Number | String | Boolean -> Boolean);
binary_operator!(greater_eq, >=, Number | String | Boolean -> Boolean);
binary_operator!(less,       <,  Number | String | Boolean -> Boolean);
binary_operator!(less_eq,    <=, Number | String | Boolean -> Boolean);
binary_operator!(minus,      -,  Number -> Number);
binary_operator!(multiply,   *,  Number -> Number);
binary_operator!(divide,     /,  Number -> Number);

fn negate<'s>(value: &Value, operator: &Token<'s>) -> ExprResult<'s> {
    use Value::*;

    match value {
        Number(n) => Ok(Number(-n)),
        _ => Err(RuntimeError::UnaryOperatorNotApplicable {
            value: value.clone(),
            operator: *operator
        })
    }
}

fn plus<'s>(lhs: &Value, rhs: &Value, operator: &Token<'s>) -> ExprResult<'s> {
    use Value::*;

    match (lhs, rhs) {
        (Number(l), Number(r)) => Ok(Number(l + r)),
        (String(l), String(r)) => Ok(String(l.clone() + r)),
        _ => Err(RuntimeError::BinaryOperatorNotApplicable{
            lhs: lhs.clone(),
            rhs: rhs.clone(),
            operator: *operator
        })
    }
}
