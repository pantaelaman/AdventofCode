use std::{collections::HashSet, io::stdin};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  println!(
    "Part 1: {}",
    lines.iter().filter(|line| supports_tls(line)).count()
  );
  println!(
    "Part 2: {}",
    lines.iter().filter(|line| supports_aba(line)).count()
  );
}

fn supports_tls(inp: &str) -> bool {
  let mut found = false;
  for (i, chunk) in inp.split(['[', ']']).enumerate() {
    let abba_present = chunk
      .chars()
      .tuple_windows()
      .any(|(a, b, c, d)| a == d && b == c && a != b);
    if i % 2 == 0 {
      found |= abba_present;
    } else if abba_present {
      return false;
    }
  }
  found
}

fn supports_aba(inp: &str) -> bool {
  let mut possible_aba: HashSet<(char, char, char)> = HashSet::new();
  for chunk in inp.split(['[', ']']).step_by(2) {
    possible_aba.extend(
      chunk
        .chars()
        .tuple_windows()
        .filter(|(a, b, c)| a == c && a != b),
    );
  }

  for chunk in inp.split(['[', ']']).skip(1).step_by(2) {
    if chunk
      .chars()
      .tuple_windows()
      .any(|(a, b, c)| a == c && possible_aba.contains(&(b, a, b)))
    {
      return true;
    }
  }
  false
}
