//! Compiler benchmarks for QB64Fresh
//!
//! These benchmarks measure the performance of the compilation pipeline.
//! Run with: `cargo bench`
//!
//! Results are saved to `target/criterion/` with HTML reports.

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use qb64fresh::codegen::{CBackend, CodeGenerator, RuntimeMode};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use qb64fresh::semantic::SemanticAnalyzer;

// =============================================================================
// Test Programs
// =============================================================================

/// A minimal "Hello World" program
const HELLO_WORLD: &str = r#"PRINT "Hello, World!""#;

/// A simple program with variables and arithmetic
const SIMPLE_MATH: &str = r#"
DIM x AS LONG
DIM y AS LONG
DIM result AS LONG
x = 10
y = 20
result = x + y * 2 - x \ 2
PRINT result
END
"#;

/// A program with a FOR loop
const FOR_LOOP: &str = r#"
DIM i AS LONG
DIM sum AS LONG
sum = 0
FOR i = 1 TO 100
    sum = sum + i
NEXT i
PRINT sum
END
"#;

/// A program with nested control flow
const NESTED_CONTROL: &str = r#"
DIM i AS LONG
DIM j AS LONG
FOR i = 1 TO 10
    FOR j = 1 TO 10
        IF i = j THEN
            PRINT i
        ELSEIF i > j THEN
            PRINT "greater"
        ELSE
            PRINT "less"
        END IF
    NEXT j
NEXT i
END
"#;

/// A program with functions and procedures
const PROCEDURES: &str = r#"
CALL DoSomething
PRINT Square(5)
PRINT DoubleIt(10)
END

SUB DoSomething
    DIM i AS LONG
    FOR i = 1 TO 5
        PRINT i
    NEXT i
END SUB

FUNCTION Square(n AS LONG) AS LONG
    Square = n * n
END FUNCTION

FUNCTION DoubleIt(n AS LONG) AS LONG
    DoubleIt = n * 2
END FUNCTION
"#;

/// A larger program combining many features
const COMPLEX_PROGRAM: &str = r#"
' Complex program combining multiple features
DIM arr(100) AS LONG
DIM i AS LONG
DIM sum AS LONG
DIM avg AS DOUBLE
DIM max AS LONG

' Initialize array
FOR i = 0 TO 100
    arr(i) = i * 2
NEXT i

' Calculate sum
sum = 0
FOR i = 0 TO 100
    sum = sum + arr(i)
NEXT i

' Calculate average
avg = sum / 101.0

' Find max
max = arr(0)
FOR i = 1 TO 100
    IF arr(i) > max THEN
        max = arr(i)
    END IF
NEXT i

' Print results
PRINT "Sum:"; sum
PRINT "Average:"; avg
PRINT "Max:"; max

END
"#;

// =============================================================================
// Benchmark Functions
// =============================================================================

/// Benchmark the lexer phase only
fn bench_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    for (name, source) in [
        ("hello_world", HELLO_WORLD),
        ("simple_math", SIMPLE_MATH),
        ("for_loop", FOR_LOOP),
        ("nested_control", NESTED_CONTROL),
        ("procedures", PROCEDURES),
        ("complex", COMPLEX_PROGRAM),
    ] {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("lex", name), source, |b, src| {
            b.iter(|| lex(black_box(src)))
        });
    }

    group.finish();
}

/// Benchmark the parser phase (lexer + parser)
fn bench_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    for (name, source) in [
        ("hello_world", HELLO_WORLD),
        ("simple_math", SIMPLE_MATH),
        ("for_loop", FOR_LOOP),
        ("nested_control", NESTED_CONTROL),
        ("procedures", PROCEDURES),
        ("complex", COMPLEX_PROGRAM),
    ] {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("parse", name), source, |b, src| {
            b.iter(|| {
                let tokens = lex(black_box(src));
                let mut parser = Parser::new(&tokens);
                parser.parse()
            })
        });
    }

    group.finish();
}

/// Benchmark the semantic analysis phase (lexer + parser + semantic)
fn bench_semantic(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantic");

    for (name, source) in [
        ("hello_world", HELLO_WORLD),
        ("simple_math", SIMPLE_MATH),
        ("for_loop", FOR_LOOP),
        ("nested_control", NESTED_CONTROL),
        ("procedures", PROCEDURES),
        ("complex", COMPLEX_PROGRAM),
    ] {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("analyze", name), source, |b, src| {
            b.iter(|| {
                let tokens = lex(black_box(src));
                let mut parser = Parser::new(&tokens);
                let program = parser.parse().unwrap();
                let mut analyzer = SemanticAnalyzer::new();
                analyzer.analyze(&program)
            })
        });
    }

    group.finish();
}

/// Benchmark the full compilation pipeline
fn bench_full_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_compilation");

    for (name, source) in [
        ("hello_world", HELLO_WORLD),
        ("simple_math", SIMPLE_MATH),
        ("for_loop", FOR_LOOP),
        ("nested_control", NESTED_CONTROL),
        ("procedures", PROCEDURES),
        ("complex", COMPLEX_PROGRAM),
    ] {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(BenchmarkId::new("compile", name), source, |b, src| {
            b.iter(|| {
                let tokens = lex(black_box(src));
                let mut parser = Parser::new(&tokens);
                let program = parser.parse().unwrap();
                let mut analyzer = SemanticAnalyzer::new();
                let typed_program = analyzer.analyze(&program).unwrap();
                let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
                backend.generate(&typed_program)
            })
        });
    }

    group.finish();
}

/// Benchmark code generation only (with pre-analyzed programs)
fn bench_codegen(c: &mut Criterion) {
    let mut group = c.benchmark_group("codegen");

    for (name, source) in [
        ("hello_world", HELLO_WORLD),
        ("simple_math", SIMPLE_MATH),
        ("for_loop", FOR_LOOP),
        ("nested_control", NESTED_CONTROL),
        ("procedures", PROCEDURES),
        ("complex", COMPLEX_PROGRAM),
    ] {
        // Pre-analyze the program
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        let typed_program = analyzer.analyze(&program).unwrap();

        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("generate", name),
            &typed_program,
            |b, prog| {
                b.iter(|| {
                    let backend = CBackend::with_runtime_mode(RuntimeMode::inline());
                    backend.generate(black_box(prog))
                })
            },
        );
    }

    group.finish();
}

// =============================================================================
// Criterion Configuration
// =============================================================================

criterion_group!(
    benches,
    bench_lexer,
    bench_parser,
    bench_semantic,
    bench_codegen,
    bench_full_compilation,
);

criterion_main!(benches);
