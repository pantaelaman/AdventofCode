#![feature(option_zip)]
use std::{
  io::stdin,
  iter::{successors, Peekable},
  str::FromStr,
};

use itertools::{
  Either::{self, Left, Right},
  Itertools,
};

struct Number {
  depth: usize,
  left: Either<u8, Box<Number>>,
  right: Either<u8, Box<Number>>,
}

struct NumberTraversal<'n> {
  queue: Vec<Either<&'n u8, &'n Number>>,
}

impl<'n> Iterator for NumberTraversal<'n> {
  type Item = &'n u8;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some(top) = self.queue.pop() {
      match top {
        Left(v) => return Some(v),
        Right(num) => {
          self.queue.push(num.right.as_ref().map_right(Box::as_ref));
          self.queue.push(num.left.as_ref().map_right(Box::as_ref));
        }
      }
    }
    None
  }
}

struct NumberTraversalMut<'n> {
  queue: Vec<Either<&'n mut u8, &'n mut Number>>,
}

impl<'n> Iterator for NumberTraversalMut<'n> {
  type Item = &'n u8;

  fn next(&mut self) -> Option<Self::Item> {
    while let Some(top) = self.queue.pop() {
      match top {
        Left(v) => return Some(v),
        Right(num) => {
          self.queue.push(num.right.as_mut().map_right(Box::as_mut));
          self.queue.push(num.left.as_mut().map_right(Box::as_mut));
        }
      }
    }
    None
  }
}

struct NumberIter<'n> {
  queue: Vec<&'n Number>,
}

impl<'n> Iterator for NumberIter<'n> {
  type Item = &'n Number;

  fn next(&mut self) -> Option<Self::Item> {
    let top = self.queue.pop()?;
    self.queue.extend(
      [top.right.as_ref().right(), top.left.as_ref().right()]
        .into_iter()
        .filter_map(|s| s)
        .map(Box::as_ref),
    );
    Some(top)
  }
}

impl Number {
  fn traverse<'n>(&'n self) -> NumberTraversal<'n> {
    NumberTraversal {
      queue: vec![Right(self)],
    }
  }

  fn traverse_mut<'n>(&'n mut self) -> NumberTraversalMut<'n> {
    NumberTraversalMut {
      queue: vec![Right(self)],
    }
  }

  fn iter<'n>(&'n self) -> NumberIter<'n> {
    NumberIter { queue: vec![self] }
  }

  fn increase_depth(&mut self) {
    let mut queue = vec![self];
    while let Some(top) = queue.pop() {
      top.depth += 1;
      queue.extend(
        [top.right.as_mut().right(), top.left.as_mut().right()]
          .into_iter()
          .filter_map(|s| s)
          .map(Box::as_mut),
      )
    }
  }

  fn leftmost_mut(&mut self) -> &mut u8 {
    let mut cur = self.left.as_mut();
    loop {
      match cur {
        Left(r) => return r,
        Right(num) => cur = num.left.as_mut(),
      }
    }
  }

  fn rightmost_mut(&mut self) -> &mut u8 {
    let mut cur = self.right.as_mut();
    loop {
      match cur {
        Left(r) => return r,
        Right(num) => cur = num.right.as_mut(),
      }
    }
  }

  fn reduce(&mut self) {
    fn explode(num: &mut Number) {}
  }
}

impl FromStr for Number {
  type Err = u8;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    fn parse_from_stream<I: Iterator<Item = char>>(
      stream: &mut Peekable<I>,
      depth: usize,
    ) -> Result<Number, u8> {
      let Some('[') = stream.next() else {
        return Err(0);
      };

      let left = match stream.peek().copied() {
        Some('[') => Right(Box::new(parse_from_stream(stream, depth + 1)?)),
        Some(c) => {
          stream.next();
          Left(c.to_digit(10).ok_or(1)? as u8)
        }
        _ => return Err(2),
      };

      let Some(',') = stream.next() else {
        return Err(3);
      };

      let right = match stream.peek().copied() {
        Some('[') => Right(Box::new(parse_from_stream(stream, depth + 1)?)),
        Some(c) => {
          stream.next();
          Left(c.to_digit(10).ok_or(4)? as u8)
        }
        _ => return Err(5),
      };

      let Some(']') = stream.next() else {
        return Err(6);
      };

      Ok(Number { depth, left, right })
    }

    parse_from_stream(&mut s.chars().peekable(), 0)
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let numbers = lines
    .iter()
    .map(|line| line.parse::<Number>().unwrap())
    .collect_vec();

  for n in numbers[3].traverse() {
    println!("{}", n);
  }
}
