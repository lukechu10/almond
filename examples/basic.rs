use coconut::ast;

fn main() {
    let ast = ast::Node {
        kind: ast::NodeKind::ReturnStatement {
            argument: Some(Box::new(ast::Node {
                kind: ast::NodeKind::Literal {
                    value: ast::LiteralValue::Number(100.0),
                },
                start: 0,
                end: 0,
            })),
        },
        start: 0,
        end: 0,
    };

    let json = serde_json::to_string(&ast).unwrap();
    println!("{}", json);
}
