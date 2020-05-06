mod utils;

use std::path::Path;
use utils::*;
use rlox::Lox;

#[test]
fn variables_can_hold_a_value_and_yield_the_value_later() {
    assert_eq!("20\n",                    lox_stdout!( var x = 20; print x ));

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
