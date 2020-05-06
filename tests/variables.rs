mod utils;

use std::path::Path;
use utils::*;
use rlox::{Lox, Value};

#[test]
fn variables_are_implicitly_initialised_to_nil() {
    assert_eq!(Some(Value::Nil),          lox!( var x; x ));
}

#[test]
fn variables_can_be_explicitly_initialised_to_a_value() {
    assert_eq!(Some(Value::Number(20.0)), lox!( var x = 20; x ));

    // Across calls to run()
    let path = Path::new("test_code");
    let mut fake_std_out: Vec<u8> = Vec::new();
    let mut lox = Lox::new(&mut fake_std_out);
    let _ = lox.run(path, "var x = 10;");
    let _ = lox.run(path, "var y = 2;");
    let _ = lox.run(path, "print x * y;");
    let output = String::from_utf8(fake_std_out).expect("Non UTF-8 output");
    assert_eq!("20\n", output);
}

#[test]
fn variables_can_vary_over_time() {
    assert_eq!("before\nafter\n",         lox_stdout!( var a = "before"; print a;
                                                       a = "after"; print a ));
}

#[test]
#[should_panic(expected = "Unresolved identifier")]
fn using_an_undeclared_variable_is_an_error() {
    lox!{ print a };
}

#[test]
fn variables_are_scoped_to_the_block_in_which_they_are_declared() {
    assert_eq!("nil\n12\nnil", lox_stdout!{
        var a;
        print a;
        {
            a = 12;
            print a;
        }
        print a;
    });
}
