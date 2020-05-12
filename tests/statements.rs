mod utils;

use utils::*;

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
        if false {
            print "Hidden";
        }

        if true {
            print "Shown";
        }
    });
}

#[test]
fn if_statements_evaluate_their_else_bodies_when_the_expression_is_falsey() {
    let expected = "Shown\n\
                    Shown\n";

    assert_eq!(expected, lox_stdout!{
        if false {
            print "Hidden";
        } else {
            print "Shown";
        }

        if true {
            print "Shown";
        } else {
            print "Hidden";
        }
    });
}

#[test]
fn if_statements_evaluate_the_truthines_of_the_result_of_an_expression() {
    let expected = "Shown\n";

    assert_eq!(expected, lox_stdout!{
        if 1 + 2 < 4 {
            print "Shown";
        } else {
            print "Hidden";
        }
    });
}

#[test]
#[should_panic(expected = "Expected '{'")]
fn if_statement_bodies_must_always_be_blocks() {
    lox!{
        if 1 != 2
            print "Hidden";
    };
}
