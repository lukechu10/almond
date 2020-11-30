//! Parse jQuery 3.5.1

fn main() {
    use almond::parser::parse_program;
    let program_str = include_str!("../benches/js/jquery-3.5.1.js");
    match parse_program(program_str.into()) {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            almond::parser::verbose_trace_dbg(program_str, &e)
        }
        // Ok(ast) => eprintln!("{}", serde_json::to_string_pretty(&ast.1).unwrap()),
        _ => {}
    }
}
