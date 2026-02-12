use crate::ast::Expr;
use crate::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::type_check::{self, Env, Typ};
use std::sync::Mutex;

// Use a mutex to ensure tests run sequentially
static TEST_LOCK: Mutex<()> = Mutex::new(());

fn parse(input: &str) -> Expr {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse()
}

fn test_eval(src: &str, expected: Expr) {
    let _lock = TEST_LOCK.lock().unwrap();
    let ast = parse(src);
    let result = eval::eval(&ast).expect(&format!("test_eval failed for: {}", src));
    assert_eq!(
        result, expected,
        "test_eval failed for: {}\nExpected: {:?}\nGot: {:?}",
        src, expected, result
    );
    println!("test_eval OK {}", src);
}

fn test_infer(src: &str, expected: Typ) {
    let _lock = TEST_LOCK.lock().unwrap();
    type_check::reset_gensym();
    let ast = parse(src);
    let env = Env::new();
    let (result, _) =
        type_check::infer(&ast, &env).expect(&format!("test_infer failed for: {}", src));
    let result = type_check::lower(&result);
    assert_eq!(
        result, expected,
        "test_infer failed for: {}\nExpected: {}\nGot: {}",
        src, expected, result
    );
    println!("test_infer OK {}", result);
}

#[test]
fn test_eval_literals() {
    test_eval("1", Expr::Int(1));
    test_eval("true", Expr::Bool(true));
}

#[test]
fn test_eval_arithmetic() {
    test_eval("0 * 1 + 2 * 4", Expr::Int(8));
}

#[test]
fn test_eval_let() {
    test_eval("let x = 1 in x", Expr::Int(1));
    test_eval("let x = 1 in x + 2", Expr::Int(3));
    test_eval("let x = 1 in x + (let y = 2 in y)", Expr::Int(3));
    test_eval("let x = 1 in x + (let x = 2 in x)", Expr::Int(3));
    test_eval("let x = 1 in x + (let y = 2 in y + x)", Expr::Int(4));
}

#[test]
fn test_eval_closure() {
    test_eval(
        "x -> x",
        Expr::Closure("x".to_string(), Box::new(Expr::Var("x".to_string()))),
    );
}

#[test]
fn test_eval_application() {
    test_eval("let first = x -> y -> x in first 1 2", Expr::Int(1));
    test_eval("let second = x -> y -> y in second 1 2", Expr::Int(2));
    test_eval("(x -> x) 1", Expr::Int(1));
    test_eval("(x -> x) (y -> y) 1", Expr::Int(1));
    test_eval("(x -> x) (x -> x) 1", Expr::Int(1));
    test_eval("let identity = x -> x in identity 1", Expr::Int(1));
    test_eval(
        "let identity = x -> x in let x = 2 in identity x",
        Expr::Int(2),
    );
    test_eval("let f = x -> y -> x + y in f 1 2", Expr::Int(3));
}

#[test]
fn test_eval_compose() {
    test_eval(
        "let add1 = x -> (x + 1) in let compose-twice = (f -> x -> f (f x)) in compose-twice add1 1",
        Expr::Int(3),
    );
}

#[test]
fn test_infer_literals() {
    test_infer("true", Typ::TBool);
    test_infer("1", Typ::TInt);
}

#[test]
fn test_infer_unary_ops() {
    test_infer("!true", Typ::TBool);
    test_infer("let x = 1 in -x", Typ::TInt);
}

#[test]
fn test_infer_let() {
    test_infer("let x = 1 in x", Typ::TInt);
}

#[test]
fn test_infer_identity() {
    test_infer("let id = x -> x in id 1", Typ::TInt);
    test_infer(
        "x -> x",
        Typ::TClosure(
            Box::new(Typ::TVar("$1".to_string())),
            Box::new(Typ::TVar("$1".to_string())),
        ),
    );
    test_infer(
        "let id = x -> x in id",
        Typ::TClosure(
            Box::new(Typ::TVar("$1".to_string())),
            Box::new(Typ::TVar("$1".to_string())),
        ),
    );
    test_infer(
        "let id = x -> x in id id",
        Typ::TClosure(
            Box::new(Typ::TVar("$1".to_string())),
            Box::new(Typ::TVar("$1".to_string())),
        ),
    );
    test_infer(
        "let id = x -> x in (id id) (id id)",
        Typ::TClosure(
            Box::new(Typ::TVar("$1".to_string())),
            Box::new(Typ::TVar("$1".to_string())),
        ),
    );
}

#[test]
fn test_infer_equality() {
    test_infer(
        "let f = x -> x = 1 in f",
        Typ::TClosure(Box::new(Typ::TInt), Box::new(Typ::TBool)),
    );
    test_infer(
        "let f = x -> x = true in f",
        Typ::TClosure(Box::new(Typ::TBool), Box::new(Typ::TBool)),
    );
    test_infer(
        "let id = x -> x in let f = x -> x = id in f",
        Typ::TClosure(
            Box::new(Typ::TClosure(
                Box::new(Typ::TVar("$1".to_string())),
                Box::new(Typ::TVar("$1".to_string())),
            )),
            Box::new(Typ::TBool),
        ),
    );
}

#[test]
fn test_infer_arithmetic_ops() {
    test_infer(
        "let negate = x -> -x in negate",
        Typ::TClosure(Box::new(Typ::TInt), Box::new(Typ::TInt)),
    );
    test_infer(
        "let id = x -> 1 * x in id",
        Typ::TClosure(Box::new(Typ::TInt), Box::new(Typ::TInt)),
    );
    test_infer(
        "let id = x -> 0 + x in id",
        Typ::TClosure(Box::new(Typ::TInt), Box::new(Typ::TInt)),
    );
}

#[test]
fn test_infer_boolean_ops() {
    test_infer(
        "let flip = x -> !x in flip",
        Typ::TClosure(Box::new(Typ::TBool), Box::new(Typ::TBool)),
    );
    test_infer(
        "let id = x -> true && x in id",
        Typ::TClosure(Box::new(Typ::TBool), Box::new(Typ::TBool)),
    );
    test_infer(
        "let id = x -> false || x in id",
        Typ::TClosure(Box::new(Typ::TBool), Box::new(Typ::TBool)),
    );
    test_infer("true && false", Typ::TBool);
    test_infer("true || false", Typ::TBool);
}

#[test]
fn test_infer_twice() {
    test_infer(
        "let twice = f -> x -> f (f x) in twice",
        Typ::TClosure(
            Box::new(Typ::TClosure(
                Box::new(Typ::TVar("$1".to_string())),
                Box::new(Typ::TVar("$1".to_string())),
            )),
            Box::new(Typ::TClosure(
                Box::new(Typ::TVar("$1".to_string())),
                Box::new(Typ::TVar("$1".to_string())),
            )),
        ),
    );
}

#[test]
fn test_infer_application() {
    test_infer(
        "f -> f (f 1)",
        Typ::TClosure(
            Box::new(Typ::TClosure(Box::new(Typ::TInt), Box::new(Typ::TInt))),
            Box::new(Typ::TInt),
        ),
    );
}

#[test]
fn test_infer_if() {
    test_infer(
        "x -> if x then 1 else 2",
        Typ::TClosure(Box::new(Typ::TBool), Box::new(Typ::TInt)),
    );
    test_infer(
        "x -> if true then 1 else x",
        Typ::TClosure(Box::new(Typ::TInt), Box::new(Typ::TInt)),
    );
}
