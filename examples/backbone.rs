//! Parse Backbone.js 1.4.0

fn main() {
    use coconut::parser::parse_program;
    let program_str = include_str!("../benches/js/backbone.js");
    match parse_program(program_str.into()) {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            coconut::parser::verbose_trace_dbg(program_str, &e)
        }
        // Ok(ast) => eprintln!("{}", serde_json::to_string_pretty(&ast.1).unwrap()),
        _ => {}
    }
}
