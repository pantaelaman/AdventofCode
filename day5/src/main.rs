use core::str;
use std::{fs::File, io::Read};

use itertools::Itertools;

fn opposites(c1: u8, c2: u8) -> bool {
  c1.to_ascii_lowercase() == c2.to_ascii_lowercase()
    && ((c1.is_ascii_lowercase() && c2.is_ascii_uppercase())
      || (c1.is_ascii_uppercase() && c2.is_ascii_lowercase()))
}

fn react(polymer: &str) -> String {
  let mut it = polymer.bytes();
  let mut finbytes = vec![it.next().unwrap()];
  for byte in it {
    let prev = match finbytes.last() {
      Some(b) => b,
      None => {
        finbytes.push(byte);
        continue;
      }
    };
    if opposites(*prev, byte) {
      finbytes.pop();
      continue;
    }

    finbytes.push(byte);
  }
  String::from_utf8(finbytes).unwrap()
}

fn main() {
  let mut file =
    File::open(std::env::args().nth(1).expect("Missing input file")).unwrap();
  let mut polymer = String::new();
  file.read_to_string(&mut polymer).unwrap();
  polymer.pop(); // take out the newline!

  let finstr = react(&polymer);
  println!("{}", finstr);
  println!("Part 1: {}", finstr.len());

  let min_len = ('a'..'z')
    .map(|problem| {
      let curpolymer = polymer
        .chars()
        .filter(|c| *c != problem && *c != problem.to_ascii_uppercase())
        .collect::<String>();
      react(&curpolymer).len()
    })
    .min()
    .unwrap();
  println!("Part 2: {}", min_len);
}
