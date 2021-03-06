#![feature(box_patterns)]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;

use std::io::{self, Write};
use std::collections::HashMap;
use std::str;
use std::sync::Mutex;
use nom::{digit};

lazy_static! {
    static ref GLOBAL_STORE: Mutex<HashMap<String, f32>> = {
        let mut m = HashMap::new();
        m.insert(String::from("ans"), 0.);
        Mutex::new(m)
    };
}

fn global_store_read(var: String) -> Result<f32, String> {
    match (*GLOBAL_STORE.lock().unwrap()).get(&var) {
        Some(num) => Ok(*num),
        None => Err(String::from(format!("{} does not exist", var)))
    }
}

fn global_store_write(var: String, val: f32) -> Option<f32> {
    (*GLOBAL_STORE.lock().unwrap()).insert(var, val)
}

#[derive(Debug)]
enum Expr {
    Arith(Box<Arith>),
    VarSet(String, Box<Arith>),
}

impl Expr {
    fn eval(self) -> Result<f32, String> {
        let res = match self {
            Expr::Arith(box arith) => arith.eval()?,
            Expr::VarSet(var_name, box arith) => {
                let res = arith.eval()?;
                global_store_write(var_name, res);
                res
            },
        };
        Ok(res)
    }
}


#[derive(Debug)]
enum Arith {
    Add(Box<Arith>, Box<Arith>),
    Sub(Box<Arith>, Box<Arith>),
    Mul(Box<Arith>, Box<Arith>),
    Div(Box<Arith>, Box<Arith>),
    Exp(Box<Arith>, Box<Arith>),
    Num(f32),
    Paren(Box<Arith>),
    Var(String),
}

impl Arith {
    fn eval(self) -> Result<f32, String> {
        let res = match self {
            Arith::Add(box a1, box a2) => a1.eval()? + a2.eval()?,
            Arith::Sub(box a1, box a2) => a1.eval()? - a2.eval()?,
            Arith::Mul(box a1, box a2) => a1.eval()? * a2.eval()?,
            Arith::Div(box a1, box a2) => a1.eval()? / a2.eval()?,
            Arith::Exp(box a1, box a2) => a1.eval()?.powf(a2.eval()?),
            Arith::Num(f) => f,
            Arith::Paren(box a1) => a1.eval()?,
            Arith::Var(name) => global_store_read(name)?
        };
        Ok(res)
    }
}


#[derive(Debug)]
enum Oper {
    Add,
    Sub,
    Mul,
    Div,
    Exp
}

named!(float<f32>,
    flat_map!(
        recognize!(
            tuple!(
                opt!(alt!(tag!("+") | tag!("-"))),
                alt_complete!(
                    delimited!(digit, tag!("."), opt!(digit))
                    | delimited!(opt!(digit), tag!("."), digit)
                    | digit
                ),
                opt!(complete!(tuple!(
                    alt!(tag!("e") | tag!("E")),
                    opt!(alt!(tag!("+") | tag!("-"))),
                    digit
                )))
            )
        ),
        parse_to!(f32)
    )
);

named!(parens< Arith >,
    delimited!(
        ws!(tag!("(")),
        map!(map!(arith, Box::new), Arith::Paren),
        ws!(tag!(")"))
    )
);

named!(factor<Arith>, alt_complete!(
    map!(ws!(float), Arith::Num) |
    map!(ws!(nom::alpha),
        |var: &[u8]| Arith::Var(str::from_utf8(var).unwrap().to_string())) |
    parens
));

fn fold_exprs(init: Arith, remainder: Vec<(Oper, Arith)>) -> Arith {
    remainder.into_iter().fold(init, |acc, (oper, arith)| {
        match oper {
            Oper::Add => Arith::Add(Box::new(acc), Box::new(arith)),
            Oper::Sub => Arith::Sub(Box::new(acc), Box::new(arith)),
            Oper::Mul => Arith::Mul(Box::new(acc), Box::new(arith)),
            Oper::Div => Arith::Div(Box::new(acc), Box::new(arith)),
            Oper::Exp => unreachable!()
        }
    })
}

fn fold_exp(base: Arith, mut exps: Vec<Arith>) -> Arith {
    // Todo: Could be prettier
    exps.insert(0, base);
    let init = exps.pop().unwrap();
    exps.into_iter().rev().fold(init, |acc, ar| {
        Arith::Exp(Box::new(ar), Box::new(acc))
    })
}

// 5*2^3^4 == 5*(2^(3^4))
named!(exp<Arith>, do_parse!(
    base: factor >>
    exps: many0!(
            do_parse!(tag!("^") >> res: factor >> (res))
        ) >>
    (fold_exp(base, exps))
));


named!(term<Arith>, do_parse!(
    init: exp >>
    remainder:
        many0!(
            alt!(
                do_parse!(tag!("*") >> mul: exp >> (Oper::Mul, mul)) |
                do_parse!(tag!("/") >> div: exp >> (Oper::Div, div))
            )
        ) >> 
    (fold_exprs(init, remainder))
));
                        
named!(arith<Arith>, do_parse!(
    init: term >>
    remainder:
        many0!(
            alt!(
                do_parse!(tag!("+") >> add: term >> (Oper::Add, add)) |
                do_parse!(tag!("-") >> sub: term >> (Oper::Sub, sub))
            )
        ) >>
    (fold_exprs(init, remainder))
));

named!(var_set<Expr>, do_parse!(
    var: map!(ws!(nom::alpha),
            |var: &[u8]| str::from_utf8(var).unwrap().to_string()) >>
    tag!("=") >>
    arith: ws!(arith) >>
    (Expr::VarSet(var, Box::new(arith)))
));


named!(expr<Expr>,
    alt_complete!(
        var_set |
        map!(map!(arith, Box::new), Expr::Arith)
));

fn main() {
    let mut buffer = String::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();
        
        {
            let expr = expr(buffer.as_bytes()).unwrap().1;
            //println!("{:?}", expr);
            let ans = expr.eval().unwrap();
            global_store_write(String::from("ans"), ans);
            println!("{}", ans);
        }
        buffer.clear();
    }
}
