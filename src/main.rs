extern crate getopts;
use getopts::Options;
use std::env;

fn do_work(input: &str) {
    println!("Hello, world!: {}", input);
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} REGEX [options]", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => {
            print_usage(&program, opts);
            panic!(f.to_string())
        }
    };

    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }
    let input = if matches.free.is_empty() {
        print_usage(&program, opts);
        panic!();
    } else {
        matches.free[0].clone()
    };
    do_work(&input);
}
