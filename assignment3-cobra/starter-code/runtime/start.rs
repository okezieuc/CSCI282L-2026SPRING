// runtime/start.rs
// This file provides the entry point for compiled programs

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name on macOS
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

fn parse_input(input: &str) -> i64 {
    match input {
        "true" => TRUE_VAL,
        "false" => FALSE_VAL,
        _ => {
            let value: i32 = input.parse().unwrap_or_else(|_| {
                eprintln!("invalid argument");
                std::process::exit(1);
            });
            (value as i64) << 1
        }
    }
}

const TRUE_VAL: i64 = 3;
const FALSE_VAL: i64 = 1;

fn print_value(value: i64) {
    match value {
        TRUE_VAL => println!("true"),
        FALSE_VAL => println!("false"),
        _ if value & 1 == 0 => println!("{}", value >> 1),
        _ => {
            eprintln!("invalid argument");
            std::process::exit(1);
        }
    }
}

#[no_mangle]
extern "C" fn snek_error(errcode: i64) {
    if errcode == 1 {
        eprintln!("invalid argument");
    } else {
        eprintln!("an error occurred");
    }
    std::process::exit(1);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let input = if args.len() > 1 {
        parse_input(&args[1])
    } else {
        FALSE_VAL
    };

    let result = unsafe { our_code_starts_here(input) };
    print_value(result);
}
