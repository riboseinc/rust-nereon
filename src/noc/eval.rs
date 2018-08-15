use super::{Value, ErrorKind};

pub fn evaluate(name: &str, _args: &[Value]) -> Result<Value, ErrorKind> {
    match name {
        "add" => unimplemented!(),
        _ => Err(ErrorKind::UnknownEval(name.to_owned())),
    }
}
