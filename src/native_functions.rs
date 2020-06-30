use crate::interpreter::{Value, NativeError};

use std::time::SystemTime;

pub fn elapsed_time(args: &Vec<Value>) -> Result<Value, NativeError> {
    if !args.is_empty() {
        return Err(NativeError::ArityMismatch(0));
    }

    let now = match SystemTime::now().elapsed() {
        Ok(duration) => {
            duration.as_secs_f64()
        }
        Err(error) => return Err(NativeError::Failed(format!("{}", error)))
    };

    Ok(Value::Number(now))
}
