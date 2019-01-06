#![feature(rust_2018_preview)]
#![feature(uniform_paths)]

extern crate getopts;
use getopts::Options;
use std::env;
use std::mem::swap;
use std::str::Chars;
use std::iter::Peekable;

mod char_class;
use char_class::CharClass;

#[derive(Clone)]
enum AST {
    Empty,
    Epsilon,
    Literal(CharClass),
    Star(Box<AST>),
    Plus(Box<AST>),
    Question(Box<AST>),
    Not(Box<AST>),
    Seq(Box<AST>, Box<AST>),
    And(Box<AST>, Box<AST>),
    Or(Box<AST>, Box<AST>),
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(Eq)]
#[derive(PartialEq)]
enum Token {
    Bare(char),
    Quoted(char),
    Class(CharClass),
}

struct TokenSeq<'a> {
    chars: &'a mut Chars<'a>,
}

impl<'a> Iterator for TokenSeq<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        match self.chars.next() {
            None => {
                return None;
            }
            Some('\\') => {
                match self.chars.next() {
                    None => {
                        panic!("Trailing backslash");
                    }
                    Some(c) => {
                        return Some(Token::Quoted(c));
                    }
                }
            }
            Some('[') => {
                match char_class::parse_char_class(&mut self.chars) {
                    Ok(cls) => {
                        return Some(Token::Class(cls));
                    }
                    Err(msg) => {
                        panic!(msg);
                    }
                }
            }
            Some(c) => {
                return Some(Token::Bare(c));
            }
        }
    }
}

fn parse_regular_expression_literal(tokens: &mut Peekable<TokenSeq>) -> Box<AST> {
    let token = tokens.peek().cloned();
    let mut a = match token {
        None => {
            return Box::new(AST::Epsilon);
        }
        Some(Token::Bare('*')) => {
            // grep(1) says this is OK
            tokens.next();
            Box::new(AST::Literal(CharClass::from_char('*')))
        }
        Some(Token::Bare('.')) => {
            tokens.next();
            Box::new(AST::Literal(CharClass::new().complement()))
        }
        Some(Token::Bare(c)) => {
            tokens.next();
            Box::new(AST::Literal(CharClass::from_char(c)))
        }
        Some(Token::Quoted(c)) if c == ')' => {
            panic!("Unmatched \\)");
        }
        Some(Token::Quoted(c)) if c == '+' || c == '?' => {
            // grep(1) says this is OK
            tokens.next();
            Box::new(AST::Literal(CharClass::from_char(c)))
        }
        Some(Token::Quoted(c)) if c == '&' || c == '|' => {
            panic!();
        }
        Some(Token::Quoted('(')) => {
            tokens.next();
            let b = parse_regular_expression_root(tokens);
            assert_eq!(tokens.next(), Some(Token::Quoted(')')));
            b
        }
        Some(Token::Quoted(c)) => {
            tokens.next();
            Box::new(AST::Literal(CharClass::from_char(c)))
        }
        Some(Token::Class(cls)) => {
            tokens.next();
            Box::new(AST::Literal(cls))
        }
    };
    loop {
        match tokens.peek() {
            Some(Token::Bare('*')) => {
                a = Box::new(AST::Star(a));
            }
            Some(Token::Quoted('+')) => {
                a = Box::new(AST::Plus(a));
            }
            Some(Token::Quoted('?')) => {
                a = Box::new(AST::Question(a));
            }
            Some(Token::Quoted('~')) => {
                a = Box::new(AST::Not(a));
            }
            _ => {
                break;
            }
        }
        tokens.next();
    }
    return a;
}

fn parse_regular_expression_root(tokens: &mut Peekable<TokenSeq>) -> Box<AST>  {
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
        match tokens.peek() {
            None | Some(Token::Bare(')')) => {
                break;
            }
            Some(Token::Quoted('|')) => {
                bump(&mut b, &mut c, AST::And);
                bump(&mut a, &mut b, AST::Or);
                tokens.next();
            }
            Some(Token::Quoted('&')) => {
                bump(&mut b, &mut c, AST::And);
                tokens.next();
            }
            Some(_) => {
                let d = parse_regular_expression_literal(tokens);
                bump(&mut c, &mut Some(d), AST::Seq);
            }
        }
    }
    bump(&mut b, &mut c, AST::And);
    bump(&mut a, &mut b, AST::Or);
    return a.unwrap();
}

fn parse_regular_expression(input: &str) -> Box<AST> {
    let mut chars = input.chars();
    let mut tokens = (TokenSeq { chars: &mut chars }).peekable();
    let a = parse_regular_expression_root(&mut tokens);
    assert!(tokens.peek().is_none());
    return a;
}

fn format_regular_expression(ast: &Box<AST>) -> (String, i8) {
    fn paren(s: String, x: i8, y: i8) -> String {
        if x <= y { s } else { "\\(".to_string() + s.as_str() + "\\)" }
    }
    match &**ast {
        AST::Empty => {
            ("\\_".to_string(), 1)
        }
        AST::Epsilon => {
            ("".to_string(), 1)
        }
        AST::Literal(cls) => {
            (cls.to_string(), 0)
        }
        AST::Star(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "*", 0)
        }
        AST::Plus(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "\\+", 0)
        }
        AST::Question(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "\\?", 0)
        }
        AST::Not(a) => {
            let (s, x) = format_regular_expression(&a);
            (paren(s, x, 0) + "\\~", 0)
        }
        AST::Seq(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 1) + paren(t, y, 1).as_str(), 1)
        }
        AST::And(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 2) + "\\&" + paren(t, y, 2).as_str(), 2)
        }
        AST::Or(a, b) => {
            let (s, x) = format_regular_expression(&a);
            let (t, y) = format_regular_expression(&b);
            (paren(s, x, 3) + "\\|" + paren(t, y, 3).as_str(), 3)
        }
    }
}

fn do_work(input: &str) {
    println!("input:  {}", input);
    let ast = parse_regular_expression(&input);
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
