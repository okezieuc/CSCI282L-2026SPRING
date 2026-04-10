// Assignment 2: Boa Compiler - Starter Code
// TODO: Complete this compiler implementation
//
// Your task is to implement a compiler for the Boa language
// that compiles expressions with let bindings to x86-64 assembly.
//
// Boa extends Adder with:
//   - Variables (identifiers)
//   - Let expressions with multiple bindings
//   - Binary operations: +, -, *

use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::prelude::*;

// ============= Abstract Syntax Tree =============

/// Unary operators
#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

/// Binary operators
#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

/// The Boa expression AST
///
/// Grammar:
///   <expr> := <number>
///           | <identifier>
///           | (let (<binding>+) <expr>)
///           | (add1 <expr>) | (sub1 <expr>)
///           | (+ <expr> <expr>) | (- <expr> <expr>) | (* <expr> <expr>)
///   <binding> := (<identifier> <expr>)
#[derive(Debug)]
enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
enum UnOp {
    Add1,
    Sub1,
    Negate,
}

#[derive(Debug, Clone)]
enum BinOp {
    Plus,
    Minus,
    Times,
}

// ============= Assembly Representation =============

/// Values that can appear in assembly instructions
#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32), // e.g., [rsp - 8]
}

/// Registers we use
#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

/// Assembly instructions we generate
#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
}

// ============= Parsing =============

/// Parse an S-expression into our Expr AST
///
/// Examples of valid Boa expressions:
///   42                          -> Number(42)
///   x                           -> Id("x")
///   (add1 5)                    -> UnOp(Add1, Number(5))
///   (+ 1 2)                     -> BinOp(Plus, Number(1), Number(2))
///   (let ((x 5)) x)             -> Let([("x", Number(5))], Id("x"))
///   (let ((x 5) (y 6)) (+ x y)) -> Let([("x", Number(5)), ("y", Number(6))], BinOp(...))
///
/// Error handling:
///   - Invalid syntax: panic!("Invalid")
///   - Number out of i32 range: panic!("Invalid")
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // TODO: Handle number atoms
        Sexp::Atom(I(n)) => Expr::Number(i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"))),

        // TODO: Handle identifier atoms
        Sexp::Atom(S(name)) => {
            if name == "let" || name == "add1" || name == "sub1" || name == "negate" {
                panic!("Invalid use of keyword as identifier: {}", name);
            }
            Expr::Id(name.to_string())
        }

        // TODO: Handle list expressions
           Sexp::List(vec) => match &vec[..] {
            // Let expression
            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                let parsed_bindings = bindings.iter().map(parse_bind).collect();
                Expr::Let(parsed_bindings, Box::new(parse_expr(body)))
            }
            
            // Unary operations
            [Sexp::Atom(S(op)), e] if op == "add1" => 
                Expr::UnOp(UnOp::Add1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => 
                Expr::UnOp(UnOp::Sub1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "negate" => 
                Expr::UnOp(UnOp::Negate, Box::new(parse_expr(e))),
            
            // Binary operations
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => 
                Expr::BinOp(BinOp::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => 
                Expr::BinOp(BinOp::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => 
                Expr::BinOp(BinOp::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
            
            _ => panic!("Invalid expression: {:?}", vec),
        },

       _ => panic!("Invalid"),
    }
}

/// Parse a single binding from a let expression
///
/// A binding looks like: (x 5) or (my-var (+ 1 2))
/// Returns a tuple of (variable_name, expression)
///
/// Error handling:
///   - Invalid binding syntax: panic!("Invalid")
fn parse_bind(s: &Sexp) -> (String, Expr) {
    // TODO: Parse a binding of the form (identifier expr)
    // Hint: match s {
    //     Sexp::List(vec) => match &vec[..] {
    //         [Sexp::Atom(S(name)), e] => (name.clone(), parse_expr(e)),
    //         _ => panic!("Invalid"),
    //     }
    //     _ => panic!("Invalid"),
    // }

    match s {
        Sexp::List(pair) => match &pair[..] {
            [Sexp::Atom(S(name)), expr] => {
                (name.to_string(), parse_expr(expr))
            }
            _ => panic!("Invalid binding: {:?}", pair),
        },
        _ => panic!("Invalid binding: {:?}", s),
    }
}

// ============= Compilation =============

/// Compile an expression to a list of assembly instructions
///
/// Parameters:
///   - e: the expression to compile
///   - si: stack index - the next available stack slot (starts at 2)
///         Stack slots are at [rsp - 8*si], e.g., si=2 means [rsp - 16]
///   - env: environment mapping variable names to stack offsets
///
/// The compiled code should leave its result in RAX.
///
/// Stack layout:
///   [rsp - 8]  : reserved (return address area)
///   [rsp - 16] : first variable (si=2)
///   [rsp - 24] : second variable (si=3)
///   ...
///
/// Examples:
///   Number(5) -> [IMov(Reg(RAX), Imm(5))]
///
///   UnOp(Add1, Number(5)) ->
///     [IMov(Reg(RAX), Imm(5)), IAdd(Reg(RAX), Imm(1))]
///
///   BinOp(Plus, Number(1), Number(2)) ->
///     1. Compile left operand (result in RAX)
///     2. Save RAX to stack at [rsp - 8*si]
///     3. Compile right operand (result in RAX)
///     4. Add stack value to RAX
///
///   Let([(x, 5)], Id(x)) ->
///     1. Compile binding expression (5)
///     2. Store result at stack slot
///     3. Add x -> stack_offset to environment
///     4. Compile body with updated environment
fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>) -> Vec<Instr> {
    match e {
        // TODO: Number - move immediate value to RAX
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],

        // TODO: Id - look up variable in environment, load from stack
        Expr::Id(name) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {}", name));
            vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, offset),
            )]
        }

        // TODO: UnOp - compile subexpression, then apply operation
        Expr::UnOp(op, expr) => {
            let mut instrs = compile_to_instrs(expr, si, env);
            match op {
                UnOp::Add1 => instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1))),
                UnOp::Sub1 => instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1))),
                UnOp::Negate => instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(-1))),
            }
            instrs
        }

        // TODO: BinOp - compile both operands using the stack
        Expr::BinOp(op, left, right) => {
            let stack_offset = -8 * si;
            let right_offset = -8 * (si + 1);
            let mut instrs = compile_to_instrs(left, si, env);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, stack_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.extend(compile_to_instrs(right, si + 1, env));

            match op {
                BinOp::Plus => instrs.push(Instr::IAdd(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, stack_offset),
                )),
                BinOp::Minus => {
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, right_offset),
                        Val::Reg(Reg::RAX),
                    ));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, stack_offset),
                    ));
                    instrs.push(Instr::ISub(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                }
                BinOp::Times => instrs.push(Instr::IMul(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, stack_offset),
                )),
            }

            instrs
        }

        // TODO: Let - bind variables and compile body
        Expr::Let(bindings, body) => {
            let mut seen = HashSet::new();
            let mut instrs = Vec::new();
            let mut next_si = si;
            let mut new_env = env.clone();

            for (name, expr) in bindings {
                if !seen.insert(name.clone()) {
                    panic!("Duplicate binding");
                }

                let stack_offset = -8 * next_si;
                instrs.extend(compile_to_instrs(expr, next_si, &new_env));
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, stack_offset),
                    Val::Reg(Reg::RAX),
                ));
                new_env.insert(name.clone(), stack_offset);
                next_si += 1;
            }

            instrs.extend(compile_to_instrs(body, next_si, &new_env));
            instrs
        }
    }
}

// ============= Code Generation =============

/// Convert a Val to its assembly string representation
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RSP) => String::from("rsp"),
        Val::Imm(n) => format!("{}", n),
        Val::RegOffset(Reg::RSP, offset) => format!("[rsp + {}]", offset),
        Val::RegOffset(Reg::RAX, offset) => format!("[rax + {}]", offset),
    }
}

/// Convert an Instr to its assembly string representation
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
    }
}

/// Compile an expression to a complete assembly string
fn compile(e: &Expr) -> String {
    let env: HashMap<String, i32> = HashMap::new();
    let instrs = compile_to_instrs(e, 2, &env);
    instrs
        .iter()
        .map(|i| instr_to_str(i))
        .collect::<Vec<String>>()
        .join("\n  ")
}

// ============= Main =============

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <input.snek> <output.s>", args[0]);
        std::process::exit(1);
    }

    let in_name = &args[1];
    let out_name = &args[2];

    // Read input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse S-expression from text
    let sexp = parse(&in_contents).unwrap_or_else(|_| panic!("Invalid"));

    // Convert S-expression to our AST
    let expr = parse_expr(&sexp);

    // Generate assembly instructions
    let instrs = compile(&expr);

    // Wrap instructions in assembly program template
    let asm_program = format!(
        "section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
",
        instrs
    );

    // Write output assembly file
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

// ============= TESTS =============
//
// Run with: cargo test
//
// These tests help verify your implementation. Uncomment and add more!

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to parse a string directly
    fn parse_str(s: &str) -> Expr {
        parse_expr(&parse(s).unwrap())
    }

    // ===== Parsing Tests =====

    #[test]
    fn test_parse_number() {
        let expr = parse_str("42");
        match expr {
            Expr::Number(42) => (),
            _ => panic!("Expected Number(42), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let expr = parse_str("x");
        match expr {
            Expr::Id(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Id(\"x\"), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_add1() {
        let expr = parse_str("(add1 5)");
        match expr {
            Expr::UnOp(UnOp::Add1, _) => (),
            _ => panic!("Expected UnOp(Add1, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_binary_plus() {
        let expr = parse_str("(+ 1 2)");
        match expr {
            Expr::BinOp(BinOp::Plus, _, _) => (),
            _ => panic!("Expected BinOp(Plus, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_let_simple() {
        let expr = parse_str("(let ((x 5)) x)");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "x");
            }
            _ => panic!("Expected Let, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_let_multiple_bindings() {
        let expr = parse_str("(let ((x 5) (y 6)) (+ x y))");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 2);
            }
            _ => panic!("Expected Let with 2 bindings, got {:?}", expr),
        }
    }

    // ===== Error Tests =====

    #[test]
    #[should_panic(expected = "Duplicate binding")]
    fn test_duplicate_binding() {
        let expr = parse_str("(let ((x 1) (x 2)) x)");
        let env: HashMap<String, i32> = HashMap::new();
        compile_to_instrs(&expr, 2, &env);
    }

    #[test]
    #[should_panic(expected = "Unbound variable identifier y")]
    fn test_unbound_variable() {
        let expr = parse_str("y");
        let env: HashMap<String, i32> = HashMap::new();
        compile_to_instrs(&expr, 2, &env);
    }

    // ===== Compilation Tests =====

    #[test]
    fn test_compile_number() {
        let expr = Expr::Number(42);
        let env: HashMap<String, i32> = HashMap::new();
        let instrs = compile_to_instrs(&expr, 2, &env);
        assert_eq!(instrs.len(), 1);
    }

    // Add more tests as you implement features!
}
