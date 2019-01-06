extern crate getopts;
use getopts::Options;
use std::env;
use std::mem::swap;
use std::str::Chars;
use std::iter::Peekable;
use std::collections::HashMap;
use std::collections::HashSet;

mod char_class;

/// Full (Regular) Regular Expression
#[derive(Clone)]
enum FRRE {
    Epsilon,
    Letter(char),
    // Class(CharClass),
    Dot,
    Star(Box<FRRE>),
    // Plus(Box<FRRE>),
    // Question(Box<FRRE>),
    Not(Box<FRRE>),
    Seq(Box<FRRE>, Box<FRRE>),
    And(Box<FRRE>, Box<FRRE>),
    Or(Box<FRRE>, Box<FRRE>),
}

/// (Regular part of) Basic Regular Expression
#[derive(Clone)]
enum RBRE {
    Epsilon,
    Letter(char),
    Class(char_class::CharClass),
    Dot,
    Star(Box<RBRE>),
    Plus(Box<RBRE>),
    Question(Box<RBRE>),
    Seq(Box<RBRE>, Box<RBRE>),
    Or(Box<RBRE>, Box<RBRE>),
}

#[derive(Clone)]
struct DFA {
}

#[derive(Clone)]
struct NFA {
}

fn parse_regular_expression_atom(s: &mut Peekable<Chars>) -> Box<FRRE> {
    match s.peek() {
        None => {
            return Box::new(FRRE::Epsilon);
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
            Box::new(FRRE::Dot)
        }
        c => {
            Box::new(FRRE::Letter(c))
        }
    };
    loop {
        if s.peek() == Some(&'*') {
            a = Box::new(FRRE::Star(a));
            s.next();
            continue;
        }
        if s.peek() == Some(&'~') {
            a = Box::new(FRRE::Not(a));
            s.next();
            continue;
        }
        break;
    }
    return a;
}

fn parse_regular_expression_root(s: &mut Peekable<Chars>) -> Box<FRRE>  {
    fn bump(a: &mut Option<Box<FRRE>>, b: &mut Option<Box<FRRE>>, op: fn(Box<FRRE>, Box<FRRE>) -> FRRE) {
        let mut a1 = None;
        let mut b1 = None;
        swap(&mut a1, a);
        swap(&mut b1, b);
        let b2 = b1.unwrap_or(Box::new(FRRE::Epsilon));
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
                bump(&mut b, &mut c, FRRE::And);
                bump(&mut a, &mut b, FRRE::Or);
                s.next();
            }
            Some('&') => {
                bump(&mut b, &mut c, FRRE::And);
                s.next();
            }
            Some(_) => {
                let d = parse_regular_expression_atom(s);
                bump(&mut c, &mut Some(d), FRRE::Seq);
            }
        }
    }
    bump(&mut b, &mut c, FRRE::And);
    bump(&mut a, &mut b, FRRE::Or);
    return a.unwrap();
}

fn parse_regular_expression(input: &str) -> Box<FRRE> {
    let mut s = input.chars().peekable();
    let a = parse_regular_expression_root(&mut s);
    assert!(s.peek().is_none());
    return a;
}

fn simplify_regular_expression(ast: &Box<FRRE>) -> Box<RBRE> {
    panic!();
}

fn compile_nondeterministic_finite_automaton(ast: &Box<FRRE>) -> NFA {
    panic!();
}

fn compile_deterministic_finite_automaton(nfa: &NFA) -> DFA {
    panic!();
}

fn minimize_deterministic_finite_automaton(dfa: &DFA) -> DFA {
    panic!();
}

fn decompile_deterministic_finite_automaton(dfa: &DFA) -> Box<FRRE> {
    panic!();
}

fn format_regular_expression(ast: &Box<FRRE>) -> (String, i8) {
    fn paren(s: String, x: i8, y: i8) -> String {
        if x <= y { s } else { "(".to_string() + s.as_str() + ")" }
    }
    match &**ast {
        FRRE::Epsilon => {
            ("".to_string(), 0)
        }
        FRRE::Letter(c) => {
            (c.to_string(), 0)
        }
        FRRE::Dot => {
            (".".to_string(), 0)
        }
        FRRE::Star(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "*", 0)
        }
        FRRE::Not(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "~", 0)
        }
        FRRE::Seq(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 1) + paren(t, y, 1).as_str(), 1)
        }
        FRRE::And(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 2) + "&" + paren(t, y, 2).as_str(), 2)
        }
        FRRE::Or(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 3) + "|" + paren(t, y, 3).as_str(), 3)
        }
    }
}

fn format_basic_regular_expression(ast: &Box<RBRE>) -> (String, i8) {
    fn paren(s: String, x: i8, y: i8) -> String {
        if x <= y { s } else { "\\(".to_string() + s.as_str() + "\\)" }
    }
    match &**ast {
        RBRE::Epsilon => {
            ("".to_string(), 0)
        }
        RBRE::Letter(c) => {
            (c.to_string(), 0)
        }
        RBRE::Class(cls) => {
            (cls.to_string(), 0)
        }
        RBRE::Dot => {
            (".".to_string(), 0)
        }
        RBRE::Star(a) => {
            let (s, x) = format_basic_regular_expression(&a);
            (paren(s, x, 0) + "*", 0)
        }
        RBRE::Plus(a) => {
            let (s, x) = format_basic_regular_expression(&a);
            (paren(s, x, 0) + "\\+", 0)
        }
        RBRE::Question(a) => {
            let (s, x) = format_basic_regular_expression(&a);
            (paren(s, x, 0) + "\\?", 0)
        }
        RBRE::Seq(a, b) => {
            let (s, x) = format_basic_regular_expression(&a);
            let (t, y) = format_basic_regular_expression(&b);
            (paren(s, x, 1) + paren(t, y, 1).as_str(), 1)
        }
        RBRE::Or(a, b) => {
            let (s, x) = format_basic_regular_expression(&a);
            let (t, y) = format_basic_regular_expression(&b);
            (paren(s, x, 2) + "\\|" + paren(t, y, 2).as_str(), 2)
        }
    }
}

fn do_work(input: &str) {
    println!("input:  {}", input);
    let ast = parse_regular_expression(&input);
    // let ast = simplify_regular_expression(&ast);
    // let nfa = compile_nondeterministic_finite_automaton(&ast);
    // let dfa = compile_deterministic_finite_automaton(&nfa);
    // let dfa = minimize_deterministic_finite_automaton(&dfa);
    // let ast = decompile_deterministic_finite_automaton(&dfa);
    let (output, _) = format_regular_expression(&ast);
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
