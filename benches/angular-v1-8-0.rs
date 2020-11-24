use criterion::{criterion_group, criterion_main, Criterion};

fn f() {
    use coconut::parser::parse_program;
    let program_str = include_str!("./js/angular-1.8.0.js");
    parse_program(program_str.into()).unwrap().1;
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("angular v1.8.0", |b| b.iter(f));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
