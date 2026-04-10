use sexp::Atom::*;
use sexp::*;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::prelude::*;

const NUM_TAG_MASK: i64 = 1;
const BOOL_TAG_MASK: i64 = 1;
const TRUE_VAL: i64 = 3;
const FALSE_VAL: i64 = 1;

fn encode_num(n: i32) -> i64 {
    (n as i64) << 1
}

#[derive(Debug)]
struct Program {
    defns: Vec<Definition>,
    main: Expr,
}

#[derive(Debug)]
struct Definition {
    name: String,
    params: Vec<String>,
    body: Expr,
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
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
enum UnOp {
    Add1,
    Sub1,
    Negate,
    IsNum,
    IsBool,
    Print,
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

#[derive(Debug, Clone, Copy)]
enum Reg {
    RAX,
    RSP,
    RBP,
    RDI,
    R10,
    R11,
    R12,
}

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
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
    IPush(Val),
    IPop(Val),
    ICall(String),
    IRet,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    arity: usize,
    label: String,
}

#[derive(Debug, Clone, Copy)]
enum CJump {
    E,
    G,
    GE,
    L,
    LE,
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
            | "isnum"
            | "isbool"
            | "print"
            | "+"
            | "-"
            | "*"
            | "<"
            | ">"
            | "<="
            | ">="
            | "="
            | "if"
            | "block"
            | "loop"
            | "break"
            | "set!"
            | "fun"
    )
}

fn parse_program_contents(contents: &str) -> Program {
    let wrapped = format!("({contents})");
    let sexp = parse(&wrapped).unwrap_or_else(|_| panic!("Invalid"));
    parse_program(&sexp)
}

fn parse_program(s: &Sexp) -> Program {
    match s {
        Sexp::List(items) => {
            let mut defns = Vec::new();
            let mut main_expr = None;

            for item in items {
                if let Some(defn) = try_parse_defn(item) {
                    if main_expr.is_some() {
                        panic!("Function definitions must appear before the main expression");
                    }
                    defns.push(defn);
                } else if main_expr.is_none() {
                    main_expr = Some(parse_expr(item));
                } else {
                    panic!("Multiple main expressions");
                }
            }

            Program {
                defns,
                main: main_expr.expect("No main expression"),
            }
        }
        _ => panic!("Invalid"),
    }
}

fn try_parse_defn(s: &Sexp) -> Option<Definition> {
    match s {
        Sexp::List(items) => match &items[..] {
            [Sexp::Atom(S(fun)), _, _] if fun == "fun" => parse_defn(s),
            _ => None,
        },
        _ => None,
    }
}

fn parse_defn(s: &Sexp) -> Option<Definition> {
    match s {
        Sexp::List(items) => match &items[..] {
            [Sexp::Atom(S(fun)), Sexp::List(signature), body] if fun == "fun" => match &signature[..] {
                [Sexp::Atom(S(name)), params @ ..] => {
                    if is_keyword(name) {
                        panic!("Invalid function name: {name}");
                    }
                    let params = params
                        .iter()
                        .map(|param| match param {
                            Sexp::Atom(S(param_name)) => {
                                if is_keyword(param_name) {
                                    panic!("Invalid parameter name: {param_name}");
                                }
                                param_name.clone()
                            }
                            _ => panic!("Invalid parameter"),
                        })
                        .collect();
                    Some(Definition {
                        name: name.clone(),
                        params,
                        body: parse_expr(body),
                    })
                }
                _ => panic!("Invalid function definition"),
            },
            _ => panic!("Invalid function definition"),
        },
        _ => panic!("Invalid function definition"),
    }
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
            Expr::Id(name.clone())
        }
        Sexp::List(items) => match &items[..] {
            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" && !bindings.is_empty() => {
                Expr::Let(bindings.iter().map(parse_bind).collect(), Box::new(parse_expr(body)))
            }
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(UnOp::Add1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(UnOp::Sub1, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "negate" => Expr::UnOp(UnOp::Negate, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(UnOp::IsNum, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(UnOp::IsBool, Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "print" => Expr::UnOp(UnOp::Print, Box::new(parse_expr(e))),
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
                Expr::Set(name.clone(), Box::new(parse_expr(value)))
            }
            [Sexp::Atom(S(name)), args @ ..] => {
                if is_keyword(name) {
                    panic!("Invalid");
                }
                Expr::Call(name.clone(), args.iter().map(parse_expr).collect())
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(items) => match &items[..] {
            [Sexp::Atom(S(name)), expr] => {
                if is_keyword(name) {
                    panic!("Invalid");
                }
                (name.clone(), parse_expr(expr))
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

fn function_label(name: &str) -> String {
    let mut label = String::from("fun_");
    for byte in name.bytes() {
        if byte.is_ascii_alphanumeric() || byte == b'_' {
            label.push(byte as char);
        } else {
            label.push_str(&format!("_{byte:02x}"));
        }
    }
    label
}

fn collect_function_info(prog: &Program) -> HashMap<String, FunctionInfo> {
    let mut functions = HashMap::new();
    for defn in &prog.defns {
        if functions.contains_key(&defn.name) {
            panic!("Duplicate function definition: {}", defn.name);
        }

        let mut seen = HashSet::new();
        for param in &defn.params {
            if !seen.insert(param.clone()) {
                panic!("Duplicate parameter: {param}");
            }
        }

        functions.insert(
            defn.name.clone(),
            FunctionInfo {
                arity: defn.params.len(),
                label: function_label(&defn.name),
            },
        );
    }
    functions
}

fn slot_offset(slot: i32) -> i32 {
    -8 * slot
}

fn align_function_frame(local_bytes: i32) -> i32 {
    if local_bytes % 16 == 0 {
        local_bytes
    } else {
        local_bytes + 8
    }
}

fn align_main_frame(local_bytes: i32) -> i32 {
    if local_bytes % 16 == 0 {
        local_bytes + 8
    } else {
        local_bytes
    }
}

fn max_stack_slot(e: &Expr, si: i32) -> i32 {
    match e {
        Expr::Number(_) | Expr::Bool(_) | Expr::Input | Expr::Id(_) => si - 1,
        Expr::UnOp(_, expr) | Expr::Loop(expr) | Expr::Break(expr) => max_stack_slot(expr, si),
        Expr::Set(_, expr) => max_stack_slot(expr, si),
        Expr::BinOp(_, left, right) => {
            let left_max = max_stack_slot(left, si);
            let right_max = max_stack_slot(right, si + 1);
            si.max(left_max).max(right_max)
        }
        Expr::Let(bindings, body) => {
            let mut max_slot = si - 1;
            let mut next_si = si;
            for (_, expr) in bindings {
                max_slot = max_slot.max(max_stack_slot(expr, next_si)).max(next_si);
                next_si += 1;
            }
            max_slot.max(max_stack_slot(body, next_si))
        }
        Expr::If(cond, thn, els) => max_stack_slot(cond, si)
            .max(max_stack_slot(thn, si))
            .max(max_stack_slot(els, si)),
        Expr::Block(exprs) => exprs
            .iter()
            .map(|expr| max_stack_slot(expr, si))
            .max()
            .unwrap_or(si - 1),
        Expr::Call(_, args) => {
            let mut max_slot = si - 1;
            let mut next_si = si;
            for arg in args.iter().rev() {
                max_slot = max_slot.max(max_stack_slot(arg, next_si)).max(next_si);
                next_si += 1;
            }
            max_slot
        }
    }
}

fn invalid_arg_instrs() -> Vec<Instr> {
    vec![Instr::IJne("throw_error_invalid".to_string())]
}

fn check_number(value: Val) -> Vec<Instr> {
    let mut instrs = vec![Instr::ITest(value, Val::Imm(NUM_TAG_MASK))];
    instrs.extend(invalid_arg_instrs());
    instrs
}

fn make_jump(kind: CJump, label: String) -> Instr {
    match kind {
        CJump::E => Instr::IJe(label),
        CJump::G => Instr::IJg(label),
        CJump::GE => Instr::IJge(label),
        CJump::L => Instr::IJl(label),
        CJump::LE => Instr::IJle(label),
    }
}

fn compile_bool_result(kind: CJump, label_counter: &mut i32) -> Vec<Instr> {
    let true_label = new_label(label_counter, "bool_true");
    let end_label = new_label(label_counter, "bool_end");
    vec![
        make_jump(kind, true_label.clone()),
        Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)),
        Instr::IJmp(end_label.clone()),
        Instr::ILabel(true_label),
        Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)),
        Instr::ILabel(end_label),
    ]
}

fn compile_expr(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    params: &HashSet<String>,
    functions: &HashMap<String, FunctionInfo>,
    label_counter: &mut i32,
    break_target: Option<&str>,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(encode_num(*n)))],
        Expr::Bool(true) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL))],
        Expr::Bool(false) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL))],
        Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R12))],
        Expr::Id(name) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {name}"));
            vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RBP, offset),
            )]
        }
        Expr::UnOp(op, expr) => {
            let mut instrs = compile_expr(expr, si, env, params, functions, label_counter, break_target);
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
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(NUM_TAG_MASK)));
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
                    instrs.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(BOOL_TAG_MASK)));
                    instrs.push(Instr::IJne(true_label.clone()));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
                    instrs.push(Instr::IJmp(end_label.clone()));
                    instrs.push(Instr::ILabel(true_label));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL)));
                    instrs.push(Instr::ILabel(end_label));
                }
                UnOp::Print => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::ICall("snek_print".to_string()));
                }
            }
            instrs
        }
        Expr::BinOp(op, left, right) => {
            let temp_offset = slot_offset(si);
            let mut instrs = compile_expr(left, si, env, params, functions, label_counter, break_target);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RBP, temp_offset),
                Val::Reg(Reg::RAX),
            ));
            instrs.extend(compile_expr(
                right,
                si + 1,
                env,
                params,
                functions,
                label_counter,
                break_target,
            ));

            match op {
                BinOp::Plus => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R10)));
                }
                BinOp::Minus => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ISub(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R10)));
                }
                BinOp::Times => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ISar(Val::Reg(Reg::R10), Val::Imm(1)));
                    instrs.push(Instr::IMul(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R10)));
                }
                BinOp::Less => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_bool_result(CJump::L, label_counter));
                }
                BinOp::Greater => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_bool_result(CJump::G, label_counter));
                }
                BinOp::LessEq => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_bool_result(CJump::LE, label_counter));
                }
                BinOp::GreaterEq => {
                    instrs.extend(check_number(Val::RegOffset(Reg::RBP, temp_offset)));
                    instrs.extend(check_number(Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_bool_result(CJump::GE, label_counter));
                }
                BinOp::Equal => {
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::R10), Val::Imm(BOOL_TAG_MASK)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::R11), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::R11), Val::Imm(BOOL_TAG_MASK)));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::R11)));
                    instrs.push(Instr::IJne("throw_error_invalid".to_string()));
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::R10),
                        Val::RegOffset(Reg::RBP, temp_offset),
                    ));
                    instrs.push(Instr::ICmp(Val::Reg(Reg::R10), Val::Reg(Reg::RAX)));
                    instrs.extend(compile_bool_result(CJump::E, label_counter));
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
                if params.contains(name) {
                    panic!("Cannot shadow parameter: {name}");
                }

                let stack_offset = slot_offset(next_si);
                instrs.extend(compile_expr(
                    expr,
                    next_si,
                    &new_env,
                    params,
                    functions,
                    label_counter,
                    break_target,
                ));
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RBP, stack_offset),
                    Val::Reg(Reg::RAX),
                ));
                new_env.insert(name.clone(), stack_offset);
                next_si += 1;
            }

            instrs.extend(compile_expr(
                body,
                next_si,
                &new_env,
                params,
                functions,
                label_counter,
                break_target,
            ));
            instrs
        }
        Expr::If(cond, thn, els) => {
            let else_label = new_label(label_counter, "if_else");
            let end_label = new_label(label_counter, "if_end");
            let mut instrs = compile_expr(cond, si, env, params, functions, label_counter, break_target);
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL)));
            instrs.push(Instr::IJe(else_label.clone()));
            instrs.extend(compile_expr(thn, si, env, params, functions, label_counter, break_target));
            instrs.push(Instr::IJmp(end_label.clone()));
            instrs.push(Instr::ILabel(else_label));
            instrs.extend(compile_expr(els, si, env, params, functions, label_counter, break_target));
            instrs.push(Instr::ILabel(end_label));
            instrs
        }
        Expr::Block(exprs) => {
            let mut instrs = Vec::new();
            for expr in exprs {
                instrs.extend(compile_expr(expr, si, env, params, functions, label_counter, break_target));
            }
            instrs
        }
        Expr::Loop(body) => {
            let start_label = new_label(label_counter, "loop_start");
            let end_label = new_label(label_counter, "loop_end");
            let mut instrs = vec![Instr::ILabel(start_label.clone())];
            instrs.extend(compile_expr(
                body,
                si,
                env,
                params,
                functions,
                label_counter,
                Some(end_label.as_str()),
            ));
            instrs.push(Instr::IJmp(start_label));
            instrs.push(Instr::ILabel(end_label));
            instrs
        }
        Expr::Break(expr) => match break_target {
            Some(target) => {
                let mut instrs = compile_expr(expr, si, env, params, functions, label_counter, break_target);
                instrs.push(Instr::IJmp(target.to_string()));
                instrs
            }
            None => panic!("break outside of loop"),
        },
        Expr::Set(name, expr) => {
            let offset = *env
                .get(name)
                .unwrap_or_else(|| panic!("Unbound variable identifier {name}"));
            let mut instrs = compile_expr(expr, si, env, params, functions, label_counter, break_target);
            instrs.push(Instr::IMov(
                Val::RegOffset(Reg::RBP, offset),
                Val::Reg(Reg::RAX),
            ));
            instrs
        }
        Expr::Call(name, args) => {
            let function = functions
                .get(name)
                .unwrap_or_else(|| panic!("Undefined function: {name}"));
            if args.len() != function.arity {
                panic!(
                    "Wrong number of arguments for {name}: expected {}, got {}",
                    function.arity,
                    args.len()
                );
            }

            let mut instrs = Vec::new();
            let mut next_si = si;
            for arg in args.iter().rev() {
                let slot = slot_offset(next_si);
                instrs.extend(compile_expr(
                    arg,
                    next_si,
                    env,
                    params,
                    functions,
                    label_counter,
                    break_target,
                ));
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RBP, slot),
                    Val::Reg(Reg::RAX),
                ));
                next_si += 1;
            }

            let pad = if args.len() % 2 == 1 { 8 } else { 0 };
            if pad > 0 {
                instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(pad as i64)));
            }

            for slot_num in si..next_si {
                instrs.push(Instr::IPush(Val::RegOffset(Reg::RBP, slot_offset(slot_num))));
            }

            instrs.push(Instr::ICall(function.label.clone()));

            let cleanup = (args.len() as i64) * 8 + pad as i64;
            if cleanup > 0 {
                instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(cleanup)));
            }

            instrs
        }
    }
}

fn compile_defn(
    defn: &Definition,
    functions: &HashMap<String, FunctionInfo>,
    label_counter: &mut i32,
) -> Vec<Instr> {
    let stack_slots = max_stack_slot(&defn.body, 1);
    let frame_bytes = align_function_frame(stack_slots * 8);

    let mut env = HashMap::new();
    let mut params = HashSet::new();
    for (index, param) in defn.params.iter().enumerate() {
        env.insert(param.clone(), 16 + index as i32 * 8);
        params.insert(param.clone());
    }

    let mut instrs = vec![
        Instr::ILabel(functions[&defn.name].label.clone()),
        Instr::IPush(Val::Reg(Reg::RBP)),
        Instr::IMov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)),
    ];

    if frame_bytes > 0 {
        instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(frame_bytes as i64)));
    }

    instrs.extend(compile_expr(
        &defn.body,
        1,
        &env,
        &params,
        functions,
        label_counter,
        None,
    ));

    if frame_bytes > 0 {
        instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(frame_bytes as i64)));
    }
    instrs.push(Instr::IPop(Val::Reg(Reg::RBP)));
    instrs.push(Instr::IRet);
    instrs
}

fn compile_main(
    expr: &Expr,
    functions: &HashMap<String, FunctionInfo>,
    label_counter: &mut i32,
) -> Vec<Instr> {
    let stack_slots = max_stack_slot(expr, 1);
    let frame_bytes = align_main_frame(stack_slots * 8);
    let env = HashMap::new();
    let params = HashSet::new();

    let mut instrs = vec![
        Instr::ILabel("our_code_starts_here".to_string()),
        Instr::IPush(Val::Reg(Reg::RBP)),
        Instr::IMov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)),
    ];

    if frame_bytes > 0 {
        instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(frame_bytes as i64)));
    }
    instrs.push(Instr::IPush(Val::Reg(Reg::R12)));
    instrs.push(Instr::IMov(Val::Reg(Reg::R12), Val::Reg(Reg::RDI)));

    instrs.extend(compile_expr(
        expr,
        1,
        &env,
        &params,
        functions,
        label_counter,
        None,
    ));

    instrs.push(Instr::IPop(Val::Reg(Reg::R12)));
    if frame_bytes > 0 {
        instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(frame_bytes as i64)));
    }
    instrs.push(Instr::IPop(Val::Reg(Reg::RBP)));
    instrs.push(Instr::IRet);
    instrs
}

fn compile_program(prog: &Program) -> String {
    let functions = collect_function_info(prog);
    let mut label_counter = 0;
    let mut instrs = vec![
        "section .text".to_string(),
        "extern snek_error".to_string(),
        "extern snek_print".to_string(),
        "global our_code_starts_here".to_string(),
    ];

    for defn in &prog.defns {
        instrs.extend(compile_defn(defn, &functions, &mut label_counter).into_iter().map(instr_to_str));
    }
    instrs.extend(compile_main(&prog.main, &functions, &mut label_counter).into_iter().map(instr_to_str));
    instrs.push("throw_error_invalid:".to_string());
    instrs.push("  mov rdi, 1".to_string());
    instrs.push("  call snek_error".to_string());

    instrs.join("\n")
}

fn reg_to_str(reg: Reg) -> &'static str {
    match reg {
        Reg::RAX => "rax",
        Reg::RSP => "rsp",
        Reg::RBP => "rbp",
        Reg::RDI => "rdi",
        Reg::R10 => "r10",
        Reg::R11 => "r11",
        Reg::R12 => "r12",
    }
}

fn val_to_str(value: &Val) -> String {
    match value {
        Val::Reg(reg) => reg_to_str(*reg).to_string(),
        Val::Imm(value) => format!("{value}"),
        Val::RegOffset(reg, offset) if *offset > 0 => {
            format!("qword [{} + {}]", reg_to_str(*reg), offset)
        }
        Val::RegOffset(reg, offset) if *offset < 0 => {
            format!("qword [{} - {}]", reg_to_str(*reg), -offset)
        }
        Val::RegOffset(reg, _) => format!("qword [{}]", reg_to_str(*reg)),
    }
}

fn instr_to_str(instr: Instr) -> String {
    match instr {
        Instr::IMov(dst, src) => format!("  mov {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::IAdd(dst, src) => format!("  add {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::ISub(dst, src) => format!("  sub {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::IMul(dst, src) => format!("  imul {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::ISar(dst, src) => format!("  sar {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::IAnd(dst, src) => format!("  and {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::ITest(dst, src) => format!("  test {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::ICmp(dst, src) => format!("  cmp {}, {}", val_to_str(&dst), val_to_str(&src)),
        Instr::ILabel(name) => format!("{name}:"),
        Instr::IJmp(label) => format!("  jmp {label}"),
        Instr::IJe(label) => format!("  je {label}"),
        Instr::IJne(label) => format!("  jne {label}"),
        Instr::IJg(label) => format!("  jg {label}"),
        Instr::IJge(label) => format!("  jge {label}"),
        Instr::IJl(label) => format!("  jl {label}"),
        Instr::IJle(label) => format!("  jle {label}"),
        Instr::IPush(value) => format!("  push {}", val_to_str(&value)),
        Instr::IPop(value) => format!("  pop {}", val_to_str(&value)),
        Instr::ICall(name) => format!("  call {name}"),
        Instr::IRet => "  ret".to_string(),
    }
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

    let program = parse_program_contents(&in_contents);
    let asm_program = compile_program(&program);

    let mut out_file = File::create(&args[2])?;
    out_file.write_all(asm_program.as_bytes())?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn parse_expr_str(source: &str) -> Expr {
        parse_expr(&parse(source).unwrap())
    }

    fn parse_program_str(source: &str) -> Program {
        parse_program_contents(source)
    }

    fn compile_str(source: &str) -> String {
        let program = parse_program_str(source);
        compile_program(&program)
    }

    fn assert_compile_error(source: &str, expected: &str) {
        let result = std::panic::catch_unwind(|| {
            let program = parse_program_contents(source);
            compile_program(&program)
        });
        match result {
            Ok(_) => panic!("expected compile error containing {expected}"),
            Err(err) => {
                let msg = if let Some(msg) = err.downcast_ref::<String>() {
                    msg.clone()
                } else if let Some(msg) = err.downcast_ref::<&str>() {
                    msg.to_string()
                } else {
                    "non-string panic".to_string()
                };
                assert!(
                    msg.contains(expected),
                    "expected panic containing {expected}, got {msg}"
                );
            }
        }
    }

    macro_rules! asm_contains_test {
        ($name:ident, $source:expr, $needle:expr) => {
            #[test]
            fn $name() {
                let asm = compile_str($source);
                assert!(
                    asm.contains($needle),
                    "expected assembly to contain {:?}\n{}",
                    $needle,
                    asm
                );
            }
        };
    }

    macro_rules! compile_error_test {
        ($name:ident, $source:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_compile_error($source, $expected);
            }
        };
    }

    #[test]
    fn parse_program_collects_definitions() {
        let program = parse_program_str(
            "
            (fun (double x) (+ x x))
            (fun (quad x) (double (double x)))
            (quad 5)
            ",
        );
        assert_eq!(program.defns.len(), 2);
        assert_eq!(program.defns[0].name, "double");
        assert_eq!(program.defns[1].name, "quad");
        match program.main {
            Expr::Call(name, args) => {
                assert_eq!(name, "quad");
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected main call expression"),
        }
    }

    #[test]
    fn parse_zero_argument_function() {
        let program = parse_program_str(
            "
            (fun (answer) 42)
            (answer)
            ",
        );
        assert_eq!(program.defns[0].params.len(), 0);
    }

    #[test]
    fn parse_call_expression() {
        let expr = parse_expr_str("(f 1 true x)");
        match expr {
            Expr::Call(name, args) => {
                assert_eq!(name, "f");
                assert_eq!(args.len(), 3);
            }
            _ => panic!("expected call"),
        }
    }

    #[test]
    fn parse_fun_body_with_let() {
        let program = parse_program_str(
            "
            (fun (compute x)
              (let ((y (* x 2))
                    (z (+ x 1)))
                (+ y z)))
            (compute 10)
            ",
        );
        match &program.defns[0].body {
            Expr::Let(bindings, _) => assert_eq!(bindings.len(), 2),
            _ => panic!("expected let body"),
        }
    }

    asm_contains_test!(simple_function_call, "
        (fun (double x) (+ x x))
        (double 5)
    ", "call fun_double");

    asm_contains_test!(zero_argument_function, "
        (fun (answer) 42)
        (answer)
    ", "call fun_answer");

    asm_contains_test!(five_argument_function, "
        (fun (add5 a b c d e) (+ (+ a b) (+ c (+ d e))))
        (add5 1 2 3 4 5)
    ", "add rsp, 48");

    asm_contains_test!(recursive_factorial, "
        (fun (factorial n)
          (if (= n 1)
              1
              (* n (factorial (- n 1)))))
        (factorial 5)
    ", "fun_factorial:");

    asm_contains_test!(mutual_recursion, "
        (fun (is-even n)
          (if (= n 0)
              true
              (is-odd (- n 1))))
        (fun (is-odd n)
          (if (= n 0)
              false
              (is-even (- n 1))))
        (is-even 10)
    ", "fun_is_2dodd:");

    asm_contains_test!(local_variables_in_function, "
        (fun (compute x)
          (let ((y (* x 2))
                (z (+ x 1)))
            (+ y z)))
        (compute 10)
    ", "[rbp - 8]");

    asm_contains_test!(mixed_parameters_and_locals, "
        (fun (mix x y)
          (let ((z (+ x y))
                (w (* y 2)))
            (- z w)))
        (mix 10 3)
    ", "[rbp + 16]");

    asm_contains_test!(print_returns_value, "
        (block
          (print 5)
          9)
    ", "call snek_print");

    asm_contains_test!(input_in_main, "
        (+ input 3)
    ", "mov r12, rdi");

    asm_contains_test!(set_parameter_inside_function, "
        (fun (advance x)
          (block
            (set! x (+ x 2))
            x))
        (advance 5)
    ", "mov qword [rbp + 16], rax");

    asm_contains_test!(loop_inside_function, "
        (fun (count-to n)
          (let ((x 0))
            (loop
              (if (= x n)
                  (break x)
                  (set! x (+ x 1))))))
        (count-to 8)
    ", "loop_start_");

    asm_contains_test!(comparison_in_function, "
        (fun (max a b)
          (if (> a b) a b))
        (max 11 4)
    ", "jg bool_true_");

    asm_contains_test!(large_arity_and_nested_temporaries, "
        (fun (combine a b c d e f)
          (+ (+ (+ a b) (+ c d)) (+ e f)))
        (combine (+ 1 2) (* 2 3) (- 10 4) (add1 7) 9 11)
    ", "[rbp - 48]");

    asm_contains_test!(fibonacci_recursive, "
        (fun (fib n)
          (if (<= n 1)
              n
              (+ (fib (- n 1)) (fib (- n 2)))))
        (fib 8)
    ", "call fun_fib");

    #[test]
    fn arithmetic_type_error_has_runtime_check() {
        let asm = compile_str(
            "
            (fun (bad x) (+ x true))
            (bad 5)
            ",
        );
        assert!(asm.contains("test rax, 1"));
        assert!(asm.contains("jne throw_error_invalid"));
    }

    #[test]
    fn equality_type_error_has_runtime_check() {
        let asm = compile_str(
            "
            (fun (bad x) (= x true))
            (bad 5)
            ",
        );
        assert!(asm.contains("and r11, 1"));
        assert!(asm.contains("jne throw_error_invalid"));
    }

    compile_error_test!(wrong_arity_error, "
        (fun (add2 x y) (+ x y))
        (add2 1)
    ", "Wrong number of arguments");

    compile_error_test!(undefined_function_error, "
        (missing 1 2)
    ", "Undefined function");

    compile_error_test!(shadow_parameter_error, "
        (fun (f x)
          (let ((x 1))
            x))
        (f 10)
    ", "Cannot shadow parameter");

    compile_error_test!(duplicate_function_error, "
        (fun (f x) x)
        (fun (f y) y)
        (f 1)
    ", "Duplicate function definition");

    compile_error_test!(duplicate_parameter_error, "
        (fun (f x x) x)
        (f 1 2)
    ", "Duplicate parameter");

    compile_error_test!(break_outside_loop_error, "
        (break 5)
    ", "break outside of loop");

    compile_error_test!(duplicate_binding_error, "
        (let ((x 1) (x 2)) x)
    ", "Duplicate binding");
}
