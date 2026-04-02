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

// DOCS FOR THE TAGGING SCHEME:
// NUMBERS ARE ENCODED BY SHIFTING LEFT ONE BIT, SO THEIR LOW BIT IS 0.
// BOOLEANS USE THE LOW BIT 1, WITH FALSE = 1 AND TRUE = 3.
// TYPE CHECKS USE THE LOW BIT MASK TO DISTINGUISH NUMBERS FROM BOOLEANS.
#[allow(dead_code)]
const NUM_TAG: i64 = 0;
const NUM_TAG_MASK: i64 = 1;
#[allow(dead_code)]
const BOOL_TAG: i64 = 1;
const BOOL_TAG_MASK: i64 = 1;
const TRUE_VAL: i64 = 3;
const FALSE_VAL: i64 = 1;

fn encode_num(n: i32) -> i64 {
    (n as i64) << 1
}

#[allow(dead_code)]
fn decode_num(tagged: i64) -> i32 {
    (tagged >> 1) as i32
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Bool(bool),
    Input,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
}

#[derive(Debug, Clone)]
enum UnOp {
    Add1,
    Sub1,
    Negate,
    IsNum,
    IsBool,
}

#[derive(Debug, Clone)]
enum BinOp {
    Plus,
    Minus,
    Times,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equal,
}

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug, Clone)]
enum Reg {
    RAX,
    RSP,
    RDI,
    R10,
}

#[derive(Debug, Clone)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ISar(Val, Val),
    IAnd(Val, Val),
    ITest(Val, Val),
    ICmp(Val, Val),
    ILabel(String),
    IJmp(String),
    IJe(String),
    IJne(String),
    IJg(String),
    IJge(String),
    IJl(String),
    IJle(String),
    ICall(String),
}

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "true"
            | "false"
            | "input"
            | "let"
            | "add1"
            | "sub1"
            | "negate"
            | "+"
            | "-"
            | "*"
            | "<"
            | ">"
            | "<="
            | ">="
            | "="
            | "isnum"
            | "isbool"
            | "if"
            | "block"
            | "loop"
            | "break"
            | "set!"
    )
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"))),
        Sexp::Atom(S(name)) if name == "true" => Expr::Bool(true),
        Sexp::Atom(S(name)) if name == "false" => Expr::Bool(false),
        Sexp::Atom(S(name)) if name == "input" => Expr::Input,
        Sexp::Atom(S(name)) => {
            if is_keyword(name) {
                panic!("Invalid");
            }
            Expr::Id(name.to_string())
        }
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" && !bindings.is_empty() => {
                Expr::Let(bindings.iter().map(parse_bind).collect(), Box::new(parse_expr(body)))
            }
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(UnOp::Add1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(UnOp::Sub1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "negate" => Expr::UnOp(UnOp::Negate, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(UnOp::IsNum, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(UnOp::IsBool, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(BinOp::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(BinOp::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(BinOp::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(BinOp::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(BinOp::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(BinOp::LessEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(BinOp::GreaterEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(BinOp::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), cond, thn, els] if op == "if" => Expr::If(
                Box::new(parse_expr(cond)),
                Box::new(parse_expr(thn)),
                Box::new(parse_expr(els)),
            ),
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" && !exprs.is_empty() => {
                Expr::Block(exprs.iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), body] if op == "loop" => Expr::Loop(Box::new(parse_expr(body))),
            [Sexp::Atom(S(op)), value] if op == "break" => Expr::Break(Box::new(parse_expr(value))),
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), value] if op == "set!" => {
                if is_keyword(name) {
                    panic!("Invalid");
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(value)))
            }
            _ => panic!("Invalid"),
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
    match s {
        Sexp::List(pair) => match &pair[..] {
            [Sexp::Atom(S(name)), expr] => {
                if is_keyword(name) {
                    panic!("Invalid");
                }
                (name.to_string(), parse_expr(expr))
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn new_label(counter: &mut i32, prefix: &str) -> String {
    let current = *counter;
    *counter += 1;
    format!("{prefix}_{current}")
}

fn invalid_arg_instrs() -> Vec<Instr> {
    vec![Instr::IJne("throw_error_invalid".to_string())]
}

fn check_number(v: Val) -> Vec<Instr> {
    let mut instrs = vec![Instr::ITest(v, Val::Imm(NUM_TAG_MASK))];
    instrs.extend(invalid_arg_instrs());
    instrs
}

fn compile_bool_result(
    jump_instr: Instr,
    label_counter: &mut i32,
) -> Vec<Instr> {
    let true_label = new_label(label_counter, "bool_true");
    let end_label = new_label(label_counter, "bool_end");
    vec![
        jump_instr_with_label(jump_instr, true_label.clone()),
        Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)),
        Instr::IJmp(end_label.clone()),
        Instr::ILabel(true_label),
        Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)),
        Instr::ILabel(end_label),
    ]
}

fn jump_instr_with_label(instr: Instr, label: String) -> Instr {
    match instr {
        Instr::IJg(_) => Instr::IJg(label),
        Instr::IJge(_) => Instr::IJge(label),
        Instr::IJl(_) => Instr::IJl(label),
        Instr::IJle(_) => Instr::IJle(label),
        Instr::IJe(_) => Instr::IJe(label),
        Instr::IJne(_) => Instr::IJne(label),
        _ => panic!("Invalid jump instruction"),
    }
}

fn compile_to_instrs(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    label_counter: &mut i32,
    break_target: Option<&str>,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(encode_num(*n)))],
        Expr::Bool(true) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL))],
        Expr::Bool(false) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL))],
        Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        Expr::Id(name) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {}", name));
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, offset))]
        }
        Expr::UnOp(op, expr) => {
            let mut instrs = compile_to_instrs(expr, si, env, label_counter, break_target);
            match op {
                UnOp::Add1 => {
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                }
                UnOp::Sub1 => {
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                }
                UnOp::Negate => {
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(-1)));
                }
                UnOp::IsNum => {
                    let false_label = new_label(label_counter, "isnum_false");
                    let end_label = new_label(label_counter, "isnum_end");
                    instrs.push(Instr::ITest(
                        Val::Reg(Reg::RAX),
                        Val::Imm(NUM_TAG_MASK),
                    ));
                    instrs.push(Instr::IJne(false_label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    instrs.push(Instr::IJmp(end_label.clone()));
                    instrs.push(Instr::ILabel(false_label));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    instrs.push(Instr::ILabel(end_label));
                }
                UnOp::IsBool => {
                    let true_label = new_label(label_counter, "isbool_true");
                    let end_label = new_label(label_counter, "isbool_end");
                    instrs.push(Instr::ITest(
                        Val::Reg(Reg::RAX),
                        Val::Imm(BOOL_TAG_MASK),
                    ));
                    instrs.push(Instr::IJne(true_label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    instrs.push(Instr::IJmp(end_label.clone()));
                    instrs.push(Instr::ILabel(true_label));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    instrs.push(Instr::ILabel(end_label));
                }
            }
            instrs
        }
        Expr::BinOp(op, left, right) => {
            let left_offset = -8 * si;
            let right_offset = -8 * (si + 1);
            let mut instrs = compile_to_instrs(left, si, env, label_counter, break_target);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, left_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.extend(compile_to_instrs(right, si + 1, env, label_counter, break_target));
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, right_offset),
                Val::Reg(Reg::RAX),
            ));

            match op {
                BinOp::Plus => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::IAdd(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                }
                BinOp::Minus => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ISub(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                }
                BinOp::Times => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                }
                BinOp::Less => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                    instrs.extend(compile_bool_result(Instr::IJl(String::new()), label_counter));
                }
                BinOp::Greater => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                    instrs.extend(compile_bool_result(Instr::IJg(String::new()), label_counter));
                }
                BinOp::LessEq => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                    instrs.extend(compile_bool_result(Instr::IJle(String::new()), label_counter));
                }
                BinOp::GreaterEq => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, left_offset)));
                    instrs.extend(check_number(Val::RegOffset(Reg::RSP, right_offset)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                    instrs.extend(compile_bool_result(Instr::IJge(String::new()), label_counter));
                }
                BinOp::Equal => {
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::IAnd(
                        Val::Reg(Reg::R10),
                        Val::Imm(BOOL_TAG_MASK),
                    ));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                    instrs.push(Instr::IAnd(
                        Val::Reg(Reg::RAX),
                        Val::Imm(BOOL_TAG_MASK),
                    ));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IJne("throw_error_invalid".to_string()));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, left_offset),
                    ));
                    instrs.push(Instr::ICmp(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, right_offset),
                    ));
                    instrs.extend(compile_bool_result(Instr::IJe(String::new()), label_counter));
                }
            }

            instrs
        }
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
                instrs.extend(compile_to_instrs(expr, next_si, &new_env, label_counter, break_target));
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RSP, stack_offset),
                    Val::Reg(Reg::RAX),
                ));
                new_env.insert(name.clone(), stack_offset);
                next_si += 1;
            }

            instrs.extend(compile_to_instrs(body, next_si, &new_env, label_counter, break_target));
            instrs
        }
        Expr::If(cond, thn, els) => {
            let else_label = new_label(label_counter, "if_else");
            let end_label = new_label(label_counter, "if_end");
            let mut instrs = compile_to_instrs(cond, si, env, label_counter, break_target);
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
            instrs.push(Instr::IJe(else_label.clone()));
            instrs.extend(compile_to_instrs(thn, si, env, label_counter, break_target));
            instrs.push(Instr::IJmp(end_label.clone()));
            instrs.push(Instr::ILabel(else_label));
            instrs.extend(compile_to_instrs(els, si, env, label_counter, break_target));
            instrs.push(Instr::ILabel(end_label));
            instrs
        }
        Expr::Block(exprs) => {
            let mut instrs = Vec::new();
            for expr in exprs {
                instrs.extend(compile_to_instrs(expr, si, env, label_counter, break_target));
            }
            instrs
        }
        Expr::Loop(body) => {
            let start_label = new_label(label_counter, "loop_start");
            let end_label = new_label(label_counter, "loop_end");
            let mut instrs = vec![Instr::ILabel(start_label.clone())];
            instrs.extend(compile_to_instrs(
                body,
                si,
                env,
                label_counter,
                Some(end_label.as_str()),
            ));
            instrs.push(Instr::IJmp(start_label));
            instrs.push(Instr::ILabel(end_label));
            instrs
        }
        Expr::Break(expr) => match break_target {
            Some(target) => {
                let mut instrs = compile_to_instrs(expr, si, env, label_counter, break_target);
                instrs.push(Instr::IJmp(target.to_string()));
                instrs
            }
            None => panic!("break outside of loop"),
        },
        Expr::Set(name, expr) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {}", name));
            let mut instrs = compile_to_instrs(expr, si, env, label_counter, break_target);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, offset),
                Val::Reg(Reg::RAX),
            ));
            instrs
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => "rax".to_string(),
        Val::Reg(Reg::RSP) => "rsp".to_string(),
        Val::Reg(Reg::RDI) => "rdi".to_string(),
        Val::Reg(Reg::R10) => "r10".to_string(),
        Val::Imm(n) => format!("{n}"),
        Val::RegOffset(Reg::RSP, offset) => format!("qword [rsp + {offset}]"),
        Val::RegOffset(Reg::RAX, offset) => format!("qword [rax + {offset}]"),
        Val::RegOffset(Reg::RDI, offset) => format!("qword [rdi + {offset}]"),
        Val::RegOffset(Reg::R10, offset) => format!("qword [r10 + {offset}]"),
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISar(dst, src) => format!("sar {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAnd(dst, src) => format!("and {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ITest(dst, src) => format!("test {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ICmp(dst, src) => format!("cmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ILabel(name) => format!("{name}:"),
        Instr::IJmp(label) => format!("jmp {label}"),
        Instr::IJe(label) => format!("je {label}"),
        Instr::IJne(label) => format!("jne {label}"),
        Instr::IJg(label) => format!("jg {label}"),
        Instr::IJge(label) => format!("jge {label}"),
        Instr::IJl(label) => format!("jl {label}"),
        Instr::IJle(label) => format!("jle {label}"),
        Instr::ICall(name) => format!("call {name}"),
    }
}

fn compile(e: &Expr) -> String {
    let env: HashMap<String, i32> = HashMap::new();
    let mut label_counter = 0;
    let mut instrs = compile_to_instrs(e, 2, &env, &mut label_counter, None);
    instrs.push(Instr::IJmp("done".to_string()));
    instrs.push(Instr::ILabel("throw_error_invalid".to_string()));
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(1)));
    instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(8)));
    instrs.push(Instr::ICall("snek_error".to_string()));
    instrs.push(Instr::ILabel("done".to_string()));
    instrs
        .iter()
        .map(instr_to_str)
        .collect::<Vec<String>>()
        .join("\n  ")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <input.snek> <output.s>", args[0]);
        std::process::exit(1);
    }

    let mut in_file = File::open(&args[1])?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = parse(&in_contents).unwrap_or_else(|_| panic!("Invalid"));
    let expr = parse_expr(&sexp);
    let instrs = compile(&expr);

    let asm_program = format!(
        "section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
  {}
  ret
",
        instrs
    );

    let mut out_file = File::create(&args[2])?;
    out_file.write_all(asm_program.as_bytes())?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_str(s: &str) -> Expr {
        parse_expr(&parse(s).unwrap())
    }

    fn compile_str(s: &str) -> String {
        compile(&parse_str(s))
    }

    #[test]
    fn test_parse_number() {
        let expr = parse_str("42");
        match expr {
            Expr::Number(42) => (),
            other => panic!("Expected Number(42), got {:?}", other),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let expr = parse_str("x");
        match expr {
            Expr::Id(name) => assert_eq!(name, "x"),
            other => panic!("Expected Id(\"x\"), got {:?}", other),
        }
    }

    #[test]
    fn test_parse_add1() {
        let expr = parse_str("(add1 5)");
        match expr {
            Expr::UnOp(UnOp::Add1, _) => (),
            other => panic!("Expected UnOp(Add1, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_parse_binary_plus() {
        let expr = parse_str("(+ 1 2)");
        match expr {
            Expr::BinOp(BinOp::Plus, _, _) => (),
            other => panic!("Expected BinOp(Plus, ...), got {:?}", other),
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
            other => panic!("Expected Let, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_let_multiple_bindings() {
        let expr = parse_str("(let ((x 5) (y 6)) (+ x y))");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 2);
            }
            other => panic!("Expected Let with 2 bindings, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_bool() {
        match parse_str("true") {
            Expr::Bool(true) => (),
            other => panic!("Expected bool, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_input() {
        match parse_str("input") {
            Expr::Input => (),
            other => panic!("Expected input, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_if() {
        match parse_str("(if true 1 0)") {
            Expr::If(_, _, _) => (),
            other => panic!("Expected if, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_loop() {
        match parse_str("(loop (break 5))") {
            Expr::Loop(_) => (),
            other => panic!("Expected loop, got {:?}", other),
        }
    }

    #[test]
    #[should_panic(expected = "Duplicate binding")]
    fn test_duplicate_binding() {
        let expr = parse_str("(let ((x 1) (x 2)) x)");
        let env: HashMap<String, i32> = HashMap::new();
        let mut labels = 0;
        compile_to_instrs(&expr, 2, &env, &mut labels, None);
    }

    #[test]
    #[should_panic(expected = "Unbound variable identifier y")]
    fn test_unbound_variable() {
        let expr = parse_str("y");
        let env: HashMap<String, i32> = HashMap::new();
        let mut labels = 0;
        compile_to_instrs(&expr, 2, &env, &mut labels, None);
    }

    #[test]
    #[should_panic(expected = "break outside of loop")]
    fn test_break_outside_loop() {
        let expr = parse_str("(break 5)");
        let env: HashMap<String, i32> = HashMap::new();
        let mut labels = 0;
        compile_to_instrs(&expr, 2, &env, &mut labels, None);
    }

    #[test]
    fn test_compile_number() {
        let expr = Expr::Number(42);
        let env: HashMap<String, i32> = HashMap::new();
        let mut labels = 0;
        let instrs = compile_to_instrs(&expr, 2, &env, &mut labels, None);
        assert_eq!(instrs.len(), 1);
    }

    #[test]
    fn test_compile_input() {
        let expr = Expr::Input;
        let env: HashMap<String, i32> = HashMap::new();
        let mut labels = 0;
        let instrs = compile_to_instrs(&expr, 2, &env, &mut labels, None);
        assert_eq!(instrs.len(), 1);
        assert!(matches!(
            instrs[0],
            Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))
        ));
    }

    #[test]
    fn test_parse_add_expr_19() {
        match parse_str("(+ (+ 6 7) 6)") {
            Expr::BinOp(BinOp::Plus, _, _) => (),
            other => panic!("Expected plus, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_add_bool_expr_int_error() {
        match parse_str("(+ true (+ 6 7))") {
            Expr::BinOp(BinOp::Plus, left, right) => {
                assert!(matches!(*left, Expr::Bool(true)));
                assert!(matches!(*right, Expr::BinOp(BinOp::Plus, _, _)));
            }
            other => panic!("Expected plus with bool lhs, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_add_int_expr_bool_error() {
        match parse_str("(+ 6 (= 6 7))") {
            Expr::BinOp(BinOp::Plus, left, right) => {
                assert!(matches!(*left, Expr::Number(6)));
                assert!(matches!(*right, Expr::BinOp(BinOp::Equal, _, _)));
            }
            other => panic!("Expected plus with equality rhs, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_comparisons() {
        assert!(matches!(parse_str("(< 6 7)"), Expr::BinOp(BinOp::Less, _, _)));
        assert!(matches!(parse_str("(> 7 6)"), Expr::BinOp(BinOp::Greater, _, _)));
        assert!(matches!(parse_str("(<= 6 6)"), Expr::BinOp(BinOp::LessEq, _, _)));
        assert!(matches!(parse_str("(>= 6 6)"), Expr::BinOp(BinOp::GreaterEq, _, _)));
        assert!(matches!(parse_str("(= 6 6)"), Expr::BinOp(BinOp::Equal, _, _)));
    }

    #[test]
    fn test_parse_nested_if() {
        match parse_str("(if true (if false 0 1) 0)") {
            Expr::If(_, then_expr, _) => {
                assert!(matches!(*then_expr, Expr::If(_, _, _)));
            }
            other => panic!("Expected nested if, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_isnum_and_isbool() {
        assert!(matches!(parse_str("(isnum 6)"), Expr::UnOp(UnOp::IsNum, _)));
        assert!(matches!(parse_str("(isbool true)"), Expr::UnOp(UnOp::IsBool, _)));
    }

    #[test]
    fn test_parse_set_simple() {
        match parse_str("(let ((x 6)) (block (set! x 7) x))") {
            Expr::Let(_, body) => match *body {
                Expr::Block(exprs) => {
                    assert!(matches!(exprs[0], Expr::Set(_, _)));
                }
                other => panic!("Expected block body, got {:?}", other),
            },
            other => panic!("Expected let, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_set_expr() {
        match parse_str("(let ((x 6)) (block (set! x (+ x 7)) x))") {
            Expr::Let(_, body) => match *body {
                Expr::Block(exprs) => match &exprs[0] {
                    Expr::Set(_, value) => {
                        assert!(matches!(**value, Expr::BinOp(BinOp::Plus, _, _)));
                    }
                    other => panic!("Expected set!, got {:?}", other),
                },
                other => panic!("Expected block body, got {:?}", other),
            },
            other => panic!("Expected let, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_loop_count_to_10() {
        match parse_str("(let ((x 1)) (loop (if (= x 10) (break x) (set! x (+ x 1)))))") {
            Expr::Let(_, body) => {
                assert!(matches!(*body, Expr::Loop(_)));
            }
            other => panic!("Expected let/loop, got {:?}", other),
        }
    }

    #[test]
    fn test_compile_add_and_sub() {
        let add = compile_str("(+ 6 7)");
        let sub = compile_str("(- 7 6)");
        assert!(add.contains("add rax"));
        assert!(sub.contains("sub rax"));
    }

    #[test]
    fn test_compile_mul() {
        let asm = compile_str("(* 6 7)");
        assert!(asm.contains("sar rax, 1"));
        assert!(asm.contains("imul rax"));
    }

    #[test]
    fn test_compile_if() {
        let asm = compile_str("(if false 1 0)");
        assert!(asm.contains("if_else_"));
        assert!(asm.contains("if_end_"));
    }

    #[test]
    fn test_compile_loop_and_break() {
        let asm = compile_str("(loop (break 5))");
        assert!(asm.contains("loop_start_"));
        assert!(asm.contains("loop_end_"));
    }

    #[test]
    fn test_compile_type_checks() {
        let isnum = compile_str("(isnum 6)");
        let isbool = compile_str("(isbool true)");
        assert!(isnum.contains("isnum_false_"));
        assert!(isbool.contains("isbool_true_"));
    }

    #[test]
    fn test_compile_set_and_block() {
        let asm = compile_str("(let ((x 6)) (block (set! x 7) x))");
        assert!(asm.contains("mov qword [rsp + -16], rax"));
    }

    #[test]
    #[should_panic(expected = "Unbound variable identifier y")]
    fn test_set_unbound_variable() {
        let expr = parse_str("(set! y 5)");
        let env: HashMap<String, i32> = HashMap::new();
        let mut labels = 0;
        compile_to_instrs(&expr, 2, &env, &mut labels, None);
    }

    #[test]
    fn test_compile_eq_type_mismatch_path() {
        let asm = compile_str("(= true 6)");
        assert!(asm.contains("throw_error_invalid"));
    }

    #[test]
    fn test_compile_input_expr() {
        let asm = compile_str("input");
        assert!(asm.contains("mov rax, rdi"));
    }
}
