extern crate getopts;
use getopts::Options;
use std::env;
use std::mem::swap;
use std::str::Chars;
use std::iter::Peekable;

enum AST {
    Epsilon,
    Letter(char),
    Dot,
    Star(Box<AST>),
    Not(Box<AST>),
    Seq(Box<AST>, Box<AST>),
    And(Box<AST>, Box<AST>),
    Or(Box<AST>, Box<AST>),
}

fn parse_regular_expression_atom(s: &mut Peekable<Chars>) -> Box<AST> {
    match s.peek() {
        None => {
            return Box::new(AST::Epsilon);
        }
        Some(c) if c == &')' || c == &'*' || c == &'&' || c == &'|' => {
            panic!();
        }
        _ => {}
    }
    let mut a = match s.next().unwrap() {
        '(' => {
            let b = parse_regular_expression_root(s);
            assert!(s.next() == Some(')'));
            b
        }
        '.' => {
            Box::new(AST::Dot)
        }
        c => {
            Box::new(AST::Letter(c))
        }
    };
    loop {
        if s.peek() == Some(&'*') {
            a = Box::new(AST::Star(a));
            s.next();
            continue;
        }
        if s.peek() == Some(&'~') {
            a = Box::new(AST::Not(a));
            s.next();
            continue;
        }
        break;
    }
    return a;
}

fn parse_regular_expression_root(s: &mut Peekable<Chars>) -> Box<AST>  {
    fn bump(a: &mut Option<Box<AST>>, b: &mut Option<Box<AST>>, op: fn(Box<AST>, Box<AST>) -> AST) {
        let mut a1 = None;
        let mut b1 = None;
        swap(&mut a1, a);
        swap(&mut b1, b);
        let b2 = b1.unwrap_or(Box::new(AST::Epsilon));
        a1 = Some(
            match a1 {
                None => { b2 }
                Some(a2) => { Box::new(op(a2, b2)) }
            }
        );
        swap(a, &mut a1);
    }
    let mut a = None;
    let mut b = None;
    let mut c = None;
    loop {
        match s.peek() {
            None | Some(')') => {
                break;
            }
            Some('|') => {
                bump(&mut b, &mut c, AST::And);
                bump(&mut a, &mut b, AST::Or);
                s.next();
            }
            Some('&') => {
                bump(&mut b, &mut c, AST::And);
                s.next();
            }
            Some(_) => {
                let d = parse_regular_expression_atom(s);
                bump(&mut c, &mut Some(d), AST::Seq);
            }
        }
    }
    bump(&mut b, &mut c, AST::And);
    bump(&mut a, &mut b, AST::Or);
    return a.unwrap();
}

fn parse_regular_expression(input: &str) -> Box<AST> {
    let mut s = input.chars().peekable();
    let a = parse_regular_expression_root(&mut s);
    assert!(s.peek().is_none());
    return a;
}

fn format_regular_expression(ast: &Box<AST>) -> (String, i8) {
    fn paren(s: String, x: i8, y: i8) -> String {
        if x <= y { s } else { "(".to_string() + s.as_str() + ")" }
    }
    match &**ast {
        AST::Epsilon => {
            ("".to_string(), 0)
        }
        AST::Letter(c) => {
            (c.to_string(), 0)
        }
        AST::Dot => {
            (".".to_string(), 0)
        }
        AST::Star(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "*", 0)
        }
        AST::Not(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "~", 0)
        }
        AST::Seq(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 1) + paren(t, y, 1).as_str(), 1)
        }
        AST::And(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 2) + "&" + paren(t, y, 2).as_str(), 2)
        }
        AST::Or(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 3) + "|" + paren(t, y, 3).as_str(), 3)
        }
    }
}

fn do_work(input: &str) {
    println!("input:  {}", input);
    let ast = parse_regular_expression(&input);
    // let dfa = compile_deterministic_finite_automaton(ast);
    // let ast2 = decompile_deterministic_finite_automaton(dfa);
    let ast2 = ast;
    let (output, _) = format_regular_expression(&ast2);
    println!("output: {}", output);
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
    do_work(&input)
}
