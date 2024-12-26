#![feature(step_trait)]
use std::{io::stdin, iter::Step};

use itertools::{iterate, Itertools};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let mut passwords =
    iterate(lines[0].to_owned(), next_password).filter(check_password);
  println!("Part 1: {}", passwords.next().unwrap());
  println!("Part 2: {}", passwords.next().unwrap());
}

fn check_password(password: &String) -> bool {
  !password.contains(&['i', 'o', 'l'])
    && password
      .chars()
      .tuple_windows()
      .filter(|(a, b, c)| {
        Step::forward(*a, 1) == *b && Step::forward(*b, 1) == *c
      })
      .count()
      > 0
    && password
      .chars()
      .tuple_windows()
      .filter_map(|(a, b)| (a == b).then_some(a))
      .dedup()
      .count()
      >= 2
}

fn next_password(password: &String) -> String {
  let mut out = String::with_capacity(password.len());
  let mut cs = password.chars().rev();
  while let Some(c) = cs.next() {
    if c == 'z' {
      out.push('a');
    } else {
      out.push(Step::forward(c, 1));
      break;
    }
  }
  out.extend(cs);
  out.chars().rev().collect()
}
