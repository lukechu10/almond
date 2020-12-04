//! Parse React development 17.0.1

fn main() {
    use almond::parse_program;
    let program_str = include_str!("../benches/js/react-development-17.0.1.js");
    match parse_program(program_str.into()) {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            almond::verbose_trace_dbg(program_str, &e)
        }
        // Ok(ast) => eprintln!("{}", serde_json::to_string_pretty(&ast.1).unwrap()),
        _ => {}
    }
}
