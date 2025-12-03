use std::io::stdin;

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let banks = lines
    .iter()
    .map(|line| {
      line
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u64)
        .collect_vec()
    })
    .collect_vec();

  let total_joltage = banks
    .iter()
    .map(|bank| {
      let pos = bank.iter().rev().position_max().unwrap();
      let i = bank.len() - pos - 1;
      if pos == 0 {
        let first = bank.iter().rev().skip(1).max().unwrap();
        first * 10 + bank[i]
      } else {
        let second = bank.iter().skip(i + 1).max().unwrap();
        bank[i] * 10 + second
      }
    })
    .sum::<u64>();

  println!("Part 1: {}", total_joltage);

  let total_joltage = banks
    .iter()
    .map(|bank| {
      let digits = seek_joltage(12, &bank);
      digits.iter().fold(0, |acc, n| acc * 10 + n)
    })
    .sum::<u64>();

  println!("Part 2: {}", total_joltage);
}

fn seek_joltage(remaining: usize, src: &[u64]) -> Vec<u64> {
  let mut buf = Vec::new();

  if remaining == 0 || src.len() == 0 {
    return buf;
  }

  let i = src.len() - src.iter().rev().position_max().unwrap() - 1;
  buf.push(src[i]);

  buf.extend(seek_joltage(remaining - 1, &src[i + 1..]));

  let new_remaining = remaining - buf.len();

  if new_remaining == 0 {
    return buf;
  }

  let mut final_buf = seek_joltage(new_remaining, &src[0..i]);
  final_buf.extend(buf);

  final_buf
}
