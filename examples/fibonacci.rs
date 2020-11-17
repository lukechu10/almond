fn main() {
    use coconut::parser::parse_program;
    let program_str = include_str!("../benches/js/fibonacci.js");
    match parse_program(program_str.into()) {
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => eprintln!("{}", e.to_string()),
        Ok(ast) => eprintln!("{}", serde_json::to_string_pretty(&ast.1).unwrap()),
        _ => {}
    }
}