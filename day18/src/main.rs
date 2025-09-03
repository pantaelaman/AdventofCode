use std::io::stdin;

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let p1 = lines
    .iter()
    .map(|line| eval_equation(line, eval_eq))
    .sum::<i64>();

  println!("Part 1: {}", p1);

  let p2 = lines
    .iter()
    .map(|line| eval_equation(line, eval_eq_prec))
    .sum::<i64>();

  println!("Part 2: {}", p2);
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Token {
  Num { val: i64 },
  LParen { width: usize },
  Op { op: Operator },
}

impl Token {
  fn as_num(self) -> i64 {
    match self {
      Self::Num { val } => val,
      _ => unimplemented!(),
    }
  }
}

impl From<i64> for Token {
  fn from(value: i64) -> Self {
    Token::Num { val: value }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Operator {
  Add,
  Mul,
}

impl Operator {
  fn apply(self, left: i64, right: i64) -> i64 {
    match self {
      Self::Add => left + right,
      Self::Mul => left * right,
    }
  }
}

fn eval_equation<F: Fn(&[Token]) -> i64>(eq: &str, evaluator: F) -> i64 {
  let mut stack = Vec::new();
  let mut active_width: usize = 0;
  for c in eq.chars().filter(|c| !c.is_whitespace()) {
    //println!("{:?}", stack);
    if let Some(d) = c.to_digit(10) {
      stack.push(Token::Num { val: d as i64 });
      active_width += 1;
    } else {
      match c {
        '+' => {
          stack.push(Token::Op { op: Operator::Add });
          active_width += 1;
        }
        '*' => {
          stack.push(Token::Op { op: Operator::Mul });
          active_width += 1;
        }
        '(' => {
          stack.push(Token::LParen {
            width: active_width,
          });
          active_width = 0;
        }
        ')' => {
          let cutoff = stack.len() - active_width;
          let value = evaluator(&stack[cutoff..]);
          stack.truncate(cutoff);
          let Token::LParen { width } = stack.pop().unwrap() else {
            unimplemented!()
          };
          active_width = width + 1;
          stack.push(value.into());
        }
        _ => unimplemented!(),
      }
    }
  }

  if active_width >= 3 {
    evaluator(&stack)
  } else if let Some(Token::Num { val }) = stack.pop() {
    val
  } else {
    unimplemented!()
  }
}

fn eval_eq(equation: &[Token]) -> i64 {
  let mut buf: [Token; 3] = equation[0..3].try_into().unwrap();
  let mut i = 3;

  loop {
    buf[0] = match buf[1] {
      Token::Op { op } => op.apply(buf[0].as_num(), buf[2].as_num()).into(),
      _ => unimplemented!(),
    };

    if i == equation.len() {
      break buf[0].as_num();
    }

    i += 2;
    buf[1] = equation[i - 2];
    buf[2] = equation[i - 1];
  }
}

fn eval_eq_prec(equation: &[Token]) -> i64 {
  let mut buf: [Token; 3] = equation[0..3].try_into().unwrap();
  let mut i = 3;
  let mut reduced = Vec::new();

  loop {
    if matches!(buf[1], Token::Op { op: Operator::Add }) {
      buf[0] = (buf[0].as_num() + buf[2].as_num()).into();
    } else {
      reduced.extend_from_slice(&buf[0..2]);
      buf[0] = buf[2];
    }
    if i == equation.len() {
      reduced.push(buf[0]);
      break;
    }

    i += 2;
    buf[1] = equation[i - 2];
    buf[2] = equation[i - 1];
  }

  reduced
    .into_iter()
    .filter_map(|token| match token {
      Token::Num { val } => Some(val),
      _ => None,
    })
    .product()
}
