use itertools::{iterate, repeat_n, Itertools};
use std::{fs::File, io::Read};

fn main() {
  let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let mut buf = String::new();
  file.read_to_string(&mut buf).unwrap();

  let digits = buf
    .trim()
    .chars()
    .map(|c| c.to_digit(10).unwrap() as i32)
    .collect_vec();

  let num_digits = digits.len() * 10000;
  let target = digits[0..7]
    .into_iter()
    .copied()
    .reduce(|p, n| p * 10 + n)
    .unwrap() as usize;
  assert!(target > num_digits / 2);

  let mut fft = iterate(digits.clone(), incr_fft);

  let result = fft.nth(100).unwrap();
  println!(
    "Part 1: {}",
    result
      .into_iter()
      .take(8)
      .reduce(|p, n| p * 10 + n)
      .unwrap()
  );

  let mut end = digits
    .into_iter()
    .cycle()
    .skip(target % num_digits)
    .take(num_digits - target)
    .collect_vec();

  for _ in 0..100 {
    let mut previous = 0;
    for digit in end.iter_mut().rev() {
      *digit = (*digit + previous) % 10;
      previous = *digit;
    }
  }

  println!(
    "Part 2: {}",
    end.into_iter().take(8).reduce(|p, n| p * 10 + n).unwrap()
  );
}

const BASE: [i32; 4] = [0, 1, 0, -1];

fn new_digit_at(index: usize, digits: &[i32]) -> i32 {
  digits
    .iter()
    .zip(pattern(index))
    .map(|(d, n)| *d * n)
    .sum::<i32>()
    .abs()
    % 10
}

fn pattern(index: usize) -> impl Iterator<Item = i32> {
  BASE
    .iter()
    .copied()
    .flat_map(move |n| repeat_n(n, index + 1))
    .cycle()
    .skip(1)
}

fn incr_fft(digits: &Vec<i32>) -> Vec<i32> {
  (0..digits.len())
    .map(|index| new_digit_at(index, &digits))
    .collect_vec()
}
