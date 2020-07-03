mod utils;

use std::path::Path;
use utils::*;
use rlox::{Lox, Value};

#[test]
fn variables_are_implicitly_initialised_to_nil() {
    assert_eq!(Some(Value::Nil),          lox!( var a; a ));
}

#[test]
fn variables_can_be_explicitly_initialised_to_a_value() {
    assert_eq!(Some(Value::Number(20.0)), lox!( var a = 20; a ));

    // Across calls to run()
    let path = Path::new("test_code");
    let mut fake_std_out: Vec<u8> = Vec::new();
    let mut lox = Lox::new(&mut fake_std_out);
    let _ = lox.run(path, "var a = 10;");
    let _ = lox.run(path, "var b = 2;");
    let _ = lox.run(path, "print a * b;");
    let output = String::from_utf8(fake_std_out).expect("Non UTF-8 output");
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
