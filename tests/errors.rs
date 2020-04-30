mod utils;

use utils::result_of;

#[test]
#[should_panic(expected = "Unterminated string")]
fn unterminated_strings_report_an_error() {
    let _ = result_of(r#" "unterminated string "#);
}
