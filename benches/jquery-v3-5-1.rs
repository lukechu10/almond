use criterion::{criterion_group, criterion_main, Criterion};

fn f() {
    use almond::parser::parse_program;
    let program_str = include_str!("./js/jquery-3.5.1.js");
    parse_program(program_str.into()).unwrap().1;
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("jquery v3.5.1", |b| b.iter(f));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
