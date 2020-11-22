use criterion::{criterion_group, criterion_main, Criterion};

fn parse_react() {
    use coconut::parser::parse_program;
    let program_str = include_str!("./js/react-development-17.0.1.js");
    parse_program(program_str.into()).unwrap().1;
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("react-development v17.0.1", |b| b.iter(|| parse_react()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
