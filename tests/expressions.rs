extern crate rlox;

use std::path::Path;

use rlox::{Lox, Value, Value::*};

#[test]
fn literals_evaluate_to_themselves() {
    assert_eq!(Nil,                       result_of(r#" nil               "#));

    assert_eq!(Number(0.0),               result_of(r#" 0                 "#));
    assert_eq!(Number(0.5),               result_of(r#" 0.5               "#));
    assert_eq!(Number(10.0),              result_of(r#" 10                "#));
    assert_eq!(Number(9999.9999),         result_of(r#" 9999.9999         "#));
    assert_eq!(Number(-1.0),              result_of(r#" -1                "#));
    assert_eq!(Number(-3.14159),          result_of(r#" -3.14159          "#));
    assert_eq!(Number(0.0),               result_of(r#" -0                "#));
    assert_eq!(Number(-1.0),              result_of(r#" -     1.0         "#));

    assert_eq!(string(""),                result_of(r#" ""                "#));
    assert_eq!(string("0"),               result_of(r#" "0"               "#));
    assert_eq!(string("Hello"),           result_of(r#" "Hello"           "#));
    assert_eq!(string("Unicode 😃"),      result_of(r#" "Unicode 😃"     "#));
    assert_eq!(string("  Spacing kept "), result_of(r#" "  Spacing kept " "#));
    assert_eq!(string("No \\n newlines"), result_of(r#" "No \n newlines"  "#));

    assert_eq!(Boolean(true),             result_of(r#" true              "#));
    assert_eq!(Boolean(false),            result_of(r#" false             "#));
}

#[test]
fn simple_arithmatic() {
    assert_eq!(Number(0.0),               result_of(r#"   0 + 0           "#));
    assert_eq!(Number(1.0),               result_of(r#"   0 + 1           "#));
    assert_eq!(Number(8.0),               result_of(r#"   2 + 6           "#));
    assert_eq!(Number(0.75),              result_of(r#" 0.5 + 0.25        "#));
    assert_eq!(Number(1.0),               result_of(r#"  -5 + 6           "#));

    assert_eq!(Number(-1.0),              result_of(r#"   0 - 1           "#));
    assert_eq!(Number(0.0),               result_of(r#"   0 - 0           "#));
    assert_eq!(Number(11.0),              result_of(r#"  10 - -1          "#));
    assert_eq!(Number(-6.0),              result_of(r#"  -8 - -2          "#));

    assert_eq!(Number(20.0),              result_of(r#"  10 * 2           "#));
    assert_eq!(Number(5.0),               result_of(r#" 0.5 * 10          "#));
    assert_eq!(Number(0.0),               result_of(r#"  -9 * 0           "#));
    assert_eq!(Number(0.0),               result_of(r#"   0 * 0           "#));

    assert_eq!(Number(5.0),               result_of(r#"  10 / 2           "#));
    assert_eq!(Number(0.125),             result_of(r#" 0.5 / 4           "#));
    assert_eq!(Number(1.0),               result_of(r#" 0.1 / 0.1         "#));
    assert_eq!(Number(0.0),               result_of(r#"   0 / 20          "#));
    assert_eq!(Number(f64::INFINITY),     result_of(r#"   8 / 0           "#));
}

#[test]
fn string_concatenation() {
    assert_eq!(string(""),                result_of(r#" "" + ""           "#));
    assert_eq!(string(" "),               result_of(r#" "" + " "          "#));
    assert_eq!(string(" hello "),         result_of(r#" " hel" + "lo "    "#));
}

#[test]
fn values_compare_equal_iff_they_are_of_the_same_type_and_magnitude() {
    let literals = ["nil", "true", "false", r#""hello""#, "10.5", "6", r#""""#];

    for (i, a) in literals.iter().enumerate() {
        for (j, b) in literals.iter().enumerate() {
            assert_eq!(Boolean(i == j),   result_of(&format!("{} == {}", a, b)));
            assert_eq!(Boolean(i != j),   result_of(&format!("{} != {}", a, b)));
        }
    }
}

#[test]
fn numbers_are_ordered_by_magnitude() {
    let numbers = ["0", "0.1", "0.5", "1", "2", "2.5", "2.55", "3.8", "3.81"];

    for (i, a) in numbers.iter().enumerate() {
        for (j, b) in numbers.iter().enumerate() {
            assert_eq!(Boolean(i > j),    result_of(&format!("{} > {}", a, b)));
            assert_eq!(Boolean(i < j),    result_of(&format!("{} < {}", a, b)));
            assert_eq!(Boolean(i >= j),   result_of(&format!("{} >= {}", a, b)));
            assert_eq!(Boolean(i <= j),   result_of(&format!("{} <= {}", a, b)));
        }
    }
}

#[test]
fn strings_are_ordered_lexographcally() {
    let strings = ["0", "1", "10", "9", "a", "b", "y", "ye", "yf", "z"];

    for (i, a) in strings.iter().enumerate() {
        for (j, b) in strings.iter().enumerate() {
            assert_eq!(Boolean(i > j),    result_of(&format!(r#""{}" > "{}""#, a, b)));
            assert_eq!(Boolean(i < j),    result_of(&format!(r#""{}" < "{}""#, a, b)));
            assert_eq!(Boolean(i >= j),   result_of(&format!(r#""{}" >= "{}""#, a, b)));
            assert_eq!(Boolean(i <= j),   result_of(&format!(r#""{}" <= "{}""#, a, b)));
        }
    }
}

#[test]
fn true_is_greater_than_false() {
    let booleans = ["false", "true"];

    for (i, a) in booleans.iter().enumerate() {
        for (j, b) in booleans.iter().enumerate() {
            assert_eq!(Boolean(i > j),    result_of(&format!(r#""{}" > "{}""#, a, b)));
            assert_eq!(Boolean(i < j),    result_of(&format!(r#""{}" < "{}""#, a, b)));
            assert_eq!(Boolean(i >= j),   result_of(&format!(r#""{}" >= "{}""#, a, b)));
            assert_eq!(Boolean(i <= j),   result_of(&format!(r#""{}" <= "{}""#, a, b)));
        }
    }
}

#[test]
fn expressions_can_be_chained() {
    assert_eq!(Number(1.5),               result_of(r#" 2 * 3 / 4         "#));
    assert_eq!(string(" .."),             result_of(r#" "" + " " + ".."   "#));
    assert_eq!(Boolean(true),             result_of(r#" 1 > 2 < true      "#));
}

fn result_of(code_snippet: &str) -> Value {
    let mut lox = Lox;

    match lox.run(Path::new("test_code"), code_snippet) {
        Ok((value, _)) => value,
        Err(error) => panic!("Code snippet failed: {}", error)
    }
}

fn string(string: &str) -> Value {
    String(string.to_owned())
}
