mod utils;

use utils::{result_of, output_of, string};

use std::path::Path;

use rlox::{Lox, Value::*};


#[test]
fn literals_evaluate_to_themselves() {
    assert_eq!(Some(Nil),                       lox!( nil               ));

    assert_eq!(Some(Number(0.0)),               lox!( 0                 ));
    assert_eq!(Some(Number(0.5)),               lox!( 0.5               ));
    assert_eq!(Some(Number(10.0)),              lox!( 10                ));
    assert_eq!(Some(Number(9999.9999)),         lox!( 9999.9999         ));
    assert_eq!(Some(Number(-1.0)),              lox!( -1                ));
    assert_eq!(Some(Number(-3.14159)),          lox!( -3.14159          ));
    assert_eq!(Some(Number(0.0)),               lox!( -0                ));
    assert_eq!(Some(Number(-1.0)),              lox!( -     1.0         ));

    assert_eq!(Some(string("")),                lox!( ""                ));
    assert_eq!(Some(string("0")),               lox!( "0"               ));
    assert_eq!(Some(string("Hello")),           lox!( "Hello"           ));
    assert_eq!(Some(string("Unicode 😃")),      lox!( "Unicode 😃"     ));
    assert_eq!(Some(string("  Spacing kept ")), lox!( "  Spacing kept " ));
    assert_eq!(Some(string("No \\n newlines")), lox!( "No \n newlines"  ));

    assert_eq!(Some(Boolean(true)),             lox!( true              ));
    assert_eq!(Some(Boolean(false)),            lox!( false             ));
}

#[test]
fn values_compare_equal_iff_they_are_of_the_same_type_and_magnitude() {
    let literals = ["nil", "true", "false", r#""hello""#, "10.5", "6", r#""""#];

    for (i, a) in literals.iter().enumerate() {
        for (j, b) in literals.iter().enumerate() {
            assert_eq!(Some(Boolean(i == j)),   result_of(&format!("{} == {}", a, b)));
            assert_eq!(Some(Boolean(i != j)),   result_of(&format!("{} != {}", a, b)));
        }
    }
}

#[test]
fn numbers_are_ordered_by_magnitude() {
    let numbers = ["0", "0.1", "0.5", "1", "2", "2.5", "2.55", "3.8", "3.81"];

    for (i, a) in numbers.iter().enumerate() {
        for (j, b) in numbers.iter().enumerate() {
            assert_eq!(Some(Boolean(i > j)),    result_of(&format!("{} > {}", a, b)));
            assert_eq!(Some(Boolean(i < j)),    result_of(&format!("{} < {}", a, b)));
            assert_eq!(Some(Boolean(i >= j)),   result_of(&format!("{} >= {}", a, b)));
            assert_eq!(Some(Boolean(i <= j)),   result_of(&format!("{} <= {}", a, b)));
        }
    }
}

#[test]
fn numbers_support_arithmatic() {
    use std::f64::INFINITY;

    assert_eq!(Some(Number(0.0)),               lox!(   0 + 0           ));
    assert_eq!(Some(Number(1.0)),               lox!(   0 + 1           ));
    assert_eq!(Some(Number(8.0)),               lox!(   2 + 6           ));
    assert_eq!(Some(Number(0.75)),              lox!( 0.5 + 0.25        ));
    assert_eq!(Some(Number(1.0)),               lox!(  -5 + 6           ));

    assert_eq!(Some(Number(-1.0)),              lox!(   0 - 1           ));
    assert_eq!(Some(Number(0.0)),               lox!(   0 - 0           ));
    assert_eq!(Some(Number(11.0)),              lox!(  10 - -1          ));
    assert_eq!(Some(Number(-6.0)),              lox!(  -8 - -2          ));

    assert_eq!(Some(Number(20.0)),              lox!(  10 * 2           ));
    assert_eq!(Some(Number(5.0)),               lox!( 0.5 * 10          ));
    assert_eq!(Some(Number(0.0)),               lox!(  -9 * 0           ));
    assert_eq!(Some(Number(0.0)),               lox!(   0 * 0           ));

    assert_eq!(Some(Number(5.0)),               lox!(  10 / 2           ));
    assert_eq!(Some(Number(0.125)),             lox!( 0.5 / 4           ));
    assert_eq!(Some(Number(1.0)),               lox!( 0.1 / 0.1         ));
    assert_eq!(Some(Number(0.0)),               lox!(   0 / 20          ));
    assert_eq!(Some(Number(INFINITY)),          lox!(   8 / 0           ));
}

#[test]
fn expressions_can_be_chained() {
    assert_eq!(Some(Number(1.5)),               lox!( 2 * 3 / 4         ));
    assert_eq!(Some(string(" ..")),             lox!( "" + " " + ".."   ));
    assert_eq!(Some(Boolean(true)),             lox!( 1 > 2 < true      ));
}

#[test]
fn groups_have_highest_precidence() {
    assert_eq!(Some(Number(9.0)),               lox!( (1 + 2) * (1 + 2) ));
    assert_eq!(Some(Number(5.0)),               lox!(  1 + (2 * 1) + 2  ));
}

#[test]
#[should_panic(expected = "Unterminated string")]
fn unterminated_strings_report_an_error() {
    let _ = result_of(r#" "unterminated string "#);
}

#[test]
fn strings_can_be_concatenated() {
    assert_eq!(Some(string("")),                lox!( "" + ""           ));
    assert_eq!(Some(string(" ")),               lox!( "" + " "          ));
    assert_eq!(Some(string(" hello ")),         lox!( " hel" + "lo "    ));
}

#[test]
fn strings_are_ordered_lexographcally() {
    let strings = ["0", "1", "10", "9", "a", "b", "y", "ye", "yf", "z"];

    for (i, a) in strings.iter().enumerate() {
        for (j, b) in strings.iter().enumerate() {
            assert_eq!(Some(Boolean(i > j)),    result_of(&format!(r#""{}" > "{}""#, a, b)));
            assert_eq!(Some(Boolean(i < j)),    result_of(&format!(r#""{}" < "{}""#, a, b)));
            assert_eq!(Some(Boolean(i >= j)),   result_of(&format!(r#""{}" >= "{}""#, a, b)));
            assert_eq!(Some(Boolean(i <= j)),   result_of(&format!(r#""{}" <= "{}""#, a, b)));
        }
    }
}

#[test]
fn true_is_greater_than_false() {
    let booleans = ["false", "true"];

    for (i, a) in booleans.iter().enumerate() {
        for (j, b) in booleans.iter().enumerate() {
            assert_eq!(Some(Boolean(i > j)),    result_of(&format!(r#""{}" > "{}""#, a, b)));
            assert_eq!(Some(Boolean(i < j)),    result_of(&format!(r#""{}" < "{}""#, a, b)));
            assert_eq!(Some(Boolean(i >= j)),   result_of(&format!(r#""{}" >= "{}""#, a, b)));
            assert_eq!(Some(Boolean(i <= j)),   result_of(&format!(r#""{}" <= "{}""#, a, b)));
        }
    }

}

#[test]
fn or_boolean_logic() {
    assert_eq!(Some(Boolean(false)),            lox!( false or false ));
    assert_eq!(Some(Boolean(true)),             lox!(  true or false ));
    assert_eq!(Some(Boolean(true)),             lox!( false or true  ));
    assert_eq!(Some(Boolean(true)),             lox!(  true or true  ));
}

#[test]
fn or_returns_first_truthy_or_last_falsey_value() {
    assert_eq!(Some(string("Second")),          lox!(     nil or "Second" ));
    assert_eq!(Some(string("First")),           lox!( "First" or "Second" ));
    assert_eq!(Some(Nil),                       lox!(   false or nil      ));
}

// Short circuiting behaviour test

#[test]
fn and_boolean_logic() {
    assert_eq!(Some(Boolean(false)),            lox!( false and false ));
    assert_eq!(Some(Boolean(false)),            lox!(  true and false ));
    assert_eq!(Some(Boolean(false)),            lox!( false and true  ));
    assert_eq!(Some(Boolean(true)),             lox!(  true and true  ));
}

#[test]
fn and_returns_first_falsey_or_last_truthy_value() {
    assert_eq!(Some(Nil),                       lox!(     nil and "Second" ));
    assert_eq!(Some(string("Second")),          lox!( "First" and "Second" ));
    assert_eq!(Some(Boolean(false)),            lox!(   false and nil      ));
}

#[test]
fn variables_are_implicitly_initialised_to_nil() {
    assert_eq!(Some(Nil),          lox!( var a; a ));
}

#[test]
fn variables_can_be_explicitly_initialised_to_a_value() {
    assert_eq!(Some(Number(20.0)), lox!( var a = 20; a ));

    // Across calls to run()
    let path = Path::new("test_code");
    let mut fake_std_out: Vec<u8> = Vec::new();
    let mut lox = Lox::new(&mut fake_std_out);
    let _ = lox.run(path, "var a = 10;");
    let _ = lox.run(path, "var b = 2;");
    let _ = lox.run(path, "print a * b;");
    let output = std::string::String::from_utf8(fake_std_out).expect("Non UTF-8 output");
    assert_eq!("20\n", output);
}

#[test]
fn variables_can_vary_over_time() {
    let expected = "before\n\
                    after\n";
    assert_eq!(expected, lox_stdout! {
        var a = "before";
        print a;
        a = "after";
        print a
    });
}

#[test]
#[should_panic(expected = "Unresolved identifier")]
fn using_an_undeclared_variable_is_an_error() {
    lox!{ print a };
}

#[test]
#[should_panic(expected = "Redeclaration")]
fn declaring_the_same_variable_more_than_once_in_a_block_is_an_error() {
    lox!{
        {
            var a = "First";
            var a = "Second";
        }
    };
}

#[test]
fn variables_are_scoped_to_the_block_in_which_they_are_declared() {
    let expected = "outer\n\
                    inner (shadowing outer)\n\
                    outer\n";
    assert_eq!(expected, lox_stdout!{
        var a = "outer";
        print a;
        {
            var a = "inner (shadowing outer)";
            print a;
        }
        print a;
    });
}

#[test]
fn variables_in_parent_scopes_can_be_mutated_in_child_scopes() {
    let expected = "outer\n\
                    inner (overwriting outer)\n\
                    inner (overwriting outer)\n";
    assert_eq!(expected, lox_stdout!{
        var a = "outer";
        print a;
        {
            a = "inner (overwriting outer)";
            print a;
        }
        print a;
    });
}

#[test]
#[should_panic(expected = "Unresolved identifier")]
fn variables_declared_in_a_scope_are_not_visible_to_parent_scopes() {
    lox!{
        "outer";
        {
            var a = "inner";
        }
        print a;
    };
}

#[test]
fn print_statements_can_print_any_type_of_value() {
    assert_eq!("Hello\n",                 lox_stdout!( print "Hello"       ));
    assert_eq!("0\n",                     lox_stdout!( print 0             ));
    assert_eq!("true\n",                  lox_stdout!( print true          ));
    assert_eq!("false\n",                 lox_stdout!( print false         ));
    assert_eq!("nil\n",                   lox_stdout!( print nil           ));
}

#[test]
fn print_statements_evaluate_expressions_and_print_the_result() {
    assert_eq!("to be\n",                 lox_stdout!( print "to " + "be"  ));
    assert_eq!("17\n",                    lox_stdout!( print 2 * 10 - 3    ));
    assert_eq!("true\n",                  lox_stdout!( print 2 > 1         ));
    assert_eq!("false\n",                 lox_stdout!( print 7 <= 6        ));
}

#[test]
#[should_panic(expected = "Missing closing delimiter")]
fn unclosed_blocks_raise_an_error() {
    result_of(r#" { "#);
}

#[test]
#[should_panic(expected = "Expected expression, found '}'")]
fn unopened_blocks_raise_an_error() {
    result_of(r#" } "#);
}

#[test]
fn if_statements_evaluate_their_bodies_when_the_expression_is_truthy() {
    let expected = "Shown\n";

    assert_eq!(expected, lox_stdout!{
        if false { print "Hidden"; }
        if true  { print "Shown";  }
    });
}

#[test]
fn if_statements_evaluate_their_else_bodies_when_the_expression_is_falsey() {
    let expected = "Shown\n\
                    Shown\n";

    assert_eq!(expected, lox_stdout!{
        if false { print "Hidden"; } else { print "Shown";  }
        if true  { print "Shown";  } else { print "Hidden"; }
    });
}

#[test]
fn if_statements_evaluate_the_truthines_of_the_result_of_an_expression() {
    let expected = "Shown\n";

    assert_eq!(expected, lox_stdout!{
        if 1 + 2 < 4 { print "Shown"; } else { print "Hidden"; }
    });
}

#[test]
#[should_panic(expected = "Expected '{'")]
fn if_statement_bodies_must_always_be_blocks() {
    lox!{ if 1 != 2 print "Hidden"; };
}

#[test]
#[should_panic(expected = "Expected '{'")]
fn while_loop_bodies_must_always_be_blocks() {
    lox!{ while true print "Shown"; };
}

#[test]
fn while_loops_execute_their_bodies_until_their_condition_becomes_falsey() {
    let expected = "1\n\
                    2\n\
                    3\n";
    assert_eq!(expected, lox_stdout!{
        var i = 0;
        while i < 3 {
            print i + 1;
            i = i + 1;
        }
    })
}

#[test]
fn while_loops_do_not_execute_their_bodies_if_their_condition_starts_falsey() {
    assert_eq!("", lox_stdout!{ while false { print "Hidden"; } });
}

#[test]
fn for_loops_provide_the_same_functionaity_in_a_more_convenient_style() {
    let expected = "1\n\
                    2\n\
                    3\n";
    assert_eq!(expected, lox_stdout!{
        for var i = 0; i < 3; i = i + 1 {
            print i + 1;
        }
    });
}

#[test]
fn for_loops_may_skip_the_initaliser() {
    let expected = "1\n\
                    2\n\
                    3\n";
    assert_eq!(expected, lox_stdout!{
        var i = 0;

        for ; i < 3; i = i + 1 {
            print i + 1;
        }
    });
}

// Missing conditions and mutators once we have a way to break an infinite loop

#[test]
fn native_calls_yield_a_value() {
    assert_eq!("number", lox!( elapsed_time(); ).unwrap().value_type());
}

#[test]
#[should_panic(expected = "Arity mismatch")]
fn native_calls_must_have_correct_arity() {
    lox!( elapsed_time("oops"); );
}

// Native function with parameters
// Native fn with incorrect types

#[test]
fn functions_can_be_defined_and_called() {
    assert_eq!("In function\n", lox_stdout!{
        fun print_this(string) {
            print string;
        }

        print_this("In function")
    });
}

#[test]
fn functions_can_return_at_any_point() {
    assert_eq!("One\n", lox_stdout!{
        fun early_return() {
            print "One";
            return;
            print "Two";
        }

        early_return()
    });
}

#[test]
fn return_statements_escape_any_enclosing_non_function_scope() {
    assert_eq!("One\n", lox_stdout!{
        fun early_return() {
            {
                print "One";
                {
                    return;
                }
            }
            print "Two";
        }

        early_return()
    });
}

#[test]
fn functions_may_return_a_value() {
    assert_eq!(Some(Number(1.0)), lox!{
        fun one() {
            return 1;
        }

        one()
    });
}

#[test]
#[should_panic(expected = "Attempt to return from global scope")]
fn returning_from_the_top_level_is_an_error() {
    lox!{ return; };
}

#[test]
fn functions_may_reference_globals() {
    assert_eq!("Global\n", lox_stdout!{
        var x = "Global";

        fun function() {
            print x;
        }

        function()
    });
}

#[test]
fn functions_may_close_over_their_environment() {
    assert_eq!("1\n2\n3\n", lox_stdout!{
        fun create_counter() {
            var count = 0;

            fun add_one() {
                count = count + 1;
                print count;
            }

            return add_one;
        }

        var bump = create_counter();
        bump(); bump(); bump();
    });
}

#[test]
fn closure_environments_are_unaffected_by_later_definitions() {
    assert_eq!("Global\nGlobal\n", lox_stdout!{
        var a = "Global";
        {
            fun showA() {
                print a;
            }

            showA();
            var a = "Local";
            showA();
        }
    });
}

#[test]
fn can_handle_recursion() {
    assert_eq!("Bottom\n", lox_stdout!{
        fun recur(i) {
            if i == 0 {
                print "Bottom";
            } else {
                recur(i - 1);
            }
        }

        recur(5);
    });
}

#[test]
fn can_run_fibanacci() {
    assert_eq!(Some(Number(6765.0)), lox!{
        fun fib(n) {
            if n <= 1 {
                return n;
            }
            return fib(n - 2) + fib(n - 1);
        }

        fib(20)
    });
}

#[test]
fn classes_print_as_their_type_name() {
    assert_eq!("<class MyType>\n", lox_stdout!{
        class MyType {}
        print MyType;
    });
}

#[test]
fn objects_may_be_instansiated_by_calling_the_type_name_as_a_constructor() {
    assert_eq!("<object MyType>\n", lox_stdout!{
        class MyType {}
        var instance = MyType();
        print instance;
    });
}

#[test]
fn object_fields_may_be_defined_via_dot_notation() {
    assert_eq!(Some(Number(2.0)), lox!{
        class MyType {}
        var instance = MyType();
        instance.value = 2;
    });
}

#[test]
fn object_fields_may_be_accessed_via_dot_notation() {
    assert_eq!(Some(Number(2.0)), lox!{
        class MyType {}
        var instance = MyType();
        instance.value = 2;
        instance.value
    });
}

#[test]
fn object_methods_may_be_called_via_dot_notation() {
    assert_eq!("Hello\n", lox_stdout!{
        class MyType {
            greet() {
                print "Hello";
            }
        }

        MyType().greet();
    });
}

#[test]
fn methods_may_use_this_to_refer_to_the_object_on_which_they_are_invoked() {
    assert_eq!("The German chocolate cake is delicious!\n", lox_stdout!{
        class Cake {
            taste() {
                var adjective = "delicious";
                print "The " + this.flavor + " cake is " + adjective + "!";
            }
        }

        var cake = Cake();
        cake.flavor = "German chocolate";
        cake.taste();
    });
}

#[test]
fn methods_are_first_class_and_remember_the_object_on_which_they_were_defined() {
    assert_eq!("Red socks\n", lox_stdout!{
        class Gift {
            callback() {
                fun localFunction() {
                    print this.colour + " socks";
                }

                return localFunction;
            }
        }

        var gift = Gift();
        var callback = gift.callback();
        gift.colour = "Red";

        callback();
    });
}

#[test]
#[should_panic(expected = "'this' cannot be used outside of an object")]
fn referencing_this_outside_of_an_object_is_an_error() {
    lox!{
        print this;
    };
}

#[test]
fn object_constructors_are_called_on_creation_if_defined() {
    assert_eq!("Red socks\n", lox_stdout!{
        class Gift {
            init() {
                this.colour = "Red";
            }

            display() {
                print this.colour + " socks";
            }
        }

        var gift = Gift();
        gift.display();
    });
}

#[test]
fn class_creation_arity_matches_constructor_arity() {
    assert_eq!("Red socks\n", lox_stdout!{
        class Gift {
            init(colour) {
                this.colour = colour;
            }

            display() {
                print this.colour + " socks";
            }
        }

        var gift = Gift("Red");
        gift.display();
    });
}

#[test]
fn calling_init_directly_reinitialises_the_object_returning_a_new_reference() {
    assert_eq!("Red socks\nBlue socks\nGreen socks\nGreen socks\n", lox_stdout!{
        class Gift {
            init(colour) {
                this.colour = colour;
            }

            display() {
                print this.colour + " socks";
            }
        }

        var gift = Gift("Red");
        gift.display();

        gift.colour = "Blue";
        gift.display();

        var reinit_gift = gift.init("Green");
        gift.display();
        reinit_gift.display();
    });
}

#[test]
fn explicitly_returning_from_a_constructor_is_allowed() {
    assert_eq!("Red\n", lox_stdout!{
        class Gift {
            init(colour) {
                this.colour = "Red";
                return;
                this.colour = colour;
            }

            display() {
                print this.colour;
            }
        }

        var gift = Gift("Blue");
        gift.display();
    });
}

#[test]
#[should_panic(expected = "Cannot return a value from a constructor")]
fn explicitly_retuning_a_value_in_a_constructor_is_an_error() {
    lox!{
        class Gift {
            init(colour) {
                return "Nonsense";
            }
        }
    };
}

#[test]
fn a_class_may_inherit_from_another() {
    assert_eq!("", lox_stdout!{
        class Parent {}

        class Child < Parent {}
    });
}

#[test]
#[should_panic(expected = "A class cannot inherit from itself")]
fn a_class_may_not_inherit_from_itself() {
    lox!{
        class Cycle < Cycle {}
    };
}

#[test]
#[should_panic(expected= "Type mismatch, expected class")]
fn a_class_may_not_inherit_from_any_other_value() {
    lox!{
        var FakeClass = "I'm not really a class";

        class MyType < FakeClass {}
    };
}

#[test]
fn methods_are_inherited_from_super_classes() {
    assert_eq!("Parent\n", lox_stdout!{
        class Parent {
            method() {
                print "Parent";
            }
        }

        class Child < Parent {}

        var instance = Child();
        instance.method();
    });
}

#[test]
fn methods_in_the_sub_class_override_those_of_the_super_class() {
    assert_eq!("Child\n", lox_stdout!{
        class Parent {
            method() {
                print "Parent";
            }
        }

        class Child < Parent {
            method() {
                print "Child";
            }
        }

        var instance = Child();
        instance.method();
    });
}

#[test]
fn super_class_methods_may_be_invoked_from_a_sub_class_with_super() {
    assert_eq!("Parent\n and Child\n", lox_stdout!{
        class Parent {
            method() {
                print "Parent";
            }
        }

        class Child < Parent {
            method() {
                super.method();
                print " and Child";
            }
        }

        var instance = Child();
        instance.method();
    });
}

#[test]
fn super_lookup_starts_on_the_class_in_which_super_is_used() {
    assert_eq!("A method\n", lox_stdout!{
        class A {
            method() {
                print "A method";
            }
        }

        class B < A {
            method() {
                print "B method";
            }

            test() {
                super.method();
            }
        }

        class C < B {}

        C().test();
    });
}

#[test]
#[should_panic(expected = "'super' can only be used within a subclass")]
fn using_super_in_a_class_that_has_no_super_class_is_an_error() {
    lox!{
        class Solo {
            method() {
                super.method();
            }
        }

        var solo = Solo();
        solo.method();
    };
}

#[test]
#[should_panic(expected = "'super' can only be used within a subclass")]
fn using_super_outside_of_a_class_is_an_error() {
    lox!{
        print super.method();
    };
}
