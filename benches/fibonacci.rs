//! A simple recursive fibonacci program in JS.
//! View ./js/fibonacic.js for JS source code.

use criterion::{criterion_group, criterion_main, Criterion};

fn f() {
    use almond::parse_program;
    let program_str = include_str!("./js/fibonacci.js");
    parse_program(program_str.into()).unwrap().1;
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci", |b| b.iter(f));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
