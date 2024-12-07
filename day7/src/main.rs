use std::{
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let tests = reader
    .lines()
    .map(|line| {
      let line = line.unwrap();
      let (p1, p2) = line.split(':').collect_tuple().unwrap();
      (
        p1.parse::<usize>().unwrap(),
        p2.trim()
          .split_whitespace()
          .map(|s| s.parse::<usize>().unwrap())
          .collect_vec(),
      )
    })
    .collect_vec();

  let total = tests
    .iter()
    .filter_map(|(test_val, nums)| {
      let num_operators = nums.len() - 1;

      (0..(2 << num_operators))
        .any(|i| {
          nums
            .iter()
            .copied()
            .enumerate()
            .reduce(|(j, num1), (k, num2)| {
              if (2 << j) & i != 0 {
                (k, num1 * num2)
              } else {
                (k, num1 + num2)
              }
            })
            .unwrap()
            .1
            == *test_val
        })
        .then_some(test_val)
    })
    .sum::<usize>();

  println!("Part 1: {}", total);

  let total = tests
    .iter()
    .filter_map(|(test_val, nums)| coalesce(*test_val, nums[0], &nums[1..]))
    .sum::<usize>();

  println!("Part 2: {}", total);
}

fn coalesce(
  target: usize,
  total_so_far: usize,
  nums: &[usize],
) -> Option<usize> {
  if nums.len() == 0 {
    return (total_so_far == target).then_some(target);
  }

  let mul_total = total_so_far * nums[0];
  let add_total = total_so_far + nums[0];
  let con_total = (total_so_far * 10usize.pow(nums[0].ilog10() + 1)) + nums[0];

  let mut possibles = Vec::new();
  if mul_total <= target {
    possibles.push(coalesce(target, mul_total, &nums[1..]));
  }
  if add_total <= target {
    possibles.push(coalesce(target, add_total, &nums[1..]));
  }
  if con_total <= target {
    possibles.push(coalesce(target, con_total, &nums[1..]));
  }

  possibles.into_iter().find(|v| v.is_some()).flatten()
}
