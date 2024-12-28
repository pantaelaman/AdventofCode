#![feature(iter_advance_by)]
use std::{io::stdin, iter::Peekable};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("");
  let size = decompress(&contents, false);
  println!("Part 1: {size}");
  let recursing_size = decompress(&contents, true);
  println!("Part 2: {recursing_size}");
}

fn eat_digit(iter: &mut Peekable<impl Iterator<Item = char>>) -> usize {
  let mut val = 0;
  while let Some(c) = iter.next_if(|c| c.is_digit(10)) {
    val = val * 10 + c.to_digit(10).unwrap() as usize;
  }
  val
}

fn decompress(inp: &str, recurse: bool) -> usize {
  let mut size = 0;
  let mut iter = inp.chars().peekable();
  while let Some(c) = iter.next() {
    if c != '(' {
      size += 1;
      continue;
    }
    let mut nchars = eat_digit(&mut iter);
    iter.next().unwrap(); // 'x'
    let times = eat_digit(&mut iter);
    iter.next().unwrap(); // ')'
    if recurse {
      let mut inner = String::with_capacity(nchars);
      for _ in 0..nchars {
        inner.push(iter.next().unwrap());
      }
      nchars = decompress(&inner, recurse);
    } else {
      iter.advance_by(nchars).unwrap();
    }
    size += times * nchars;
  }
  size
}
