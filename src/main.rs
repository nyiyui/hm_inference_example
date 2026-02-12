mod ast;
mod eval;
mod lexer;
mod parser;
mod type_check;

use std::env;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        // Run with command line argument
        let input = &args[1];
        run_program(input);
    } else {
        // Interactive REPL mode
        repl();
    }
}

fn repl() {
    println!("Hindley-Milner Type Inference Example (Rust Edition)");
    println!("Type expressions to evaluate and infer types. Press Ctrl+C to exit.");
    println!();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            break;
        }

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        run_program(input);
        println!();
    }
}

fn run_program(input: &str) {
    // Tokenize
    let mut lexer = lexer::Lexer::new(input);
    let tokens = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| lexer.tokenize())) {
        Ok(tokens) => tokens,
        Err(_) => {
            println!("Error: Failed to tokenize input");
            return;
        }
    };

    // Parse
    let mut parser = parser::Parser::new(tokens);
    let ast = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.parse())) {
        Ok(ast) => ast,
        Err(_) => {
            println!("Error: Failed to parse input");
            return;
        }
    };

    // Type check
    type_check::reset_gensym();
    let env = type_check::Env::new();
    let result_type = match type_check::infer(&ast, &env) {
        Ok((t, _)) => type_check::lower(&t),
        Err(e) => {
            println!("Type error: {}", e);
            return;
        }
    };

    // Evaluate
    let result = match eval::eval(&ast) {
        Ok(r) => r,
        Err(e) => {
            println!("Evaluation error: {}", e);
            return;
        }
    };

    println!("Result: {}", result);
    println!("Type: {}", result_type);
}

#[cfg(test)]
mod tests;
