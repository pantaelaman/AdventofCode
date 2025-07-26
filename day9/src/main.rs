use std::{collections::VecDeque, io::stdin};

use itertools::{Itertools, MinMaxResult};

const PREAMBLE_LENGTH: usize = 25;

#[derive(Default)]
struct SumBuffer {
  buffers: VecDeque<(usize, Vec<usize>)>,
}

impl SumBuffer {
  fn build(&mut self, new: usize) {
    for buffer in self.buffers.iter_mut() {
      buffer.1.push(buffer.0 + new);
    }

    self.buffers.push_back((new, Vec::new()));
  }

  fn next(&mut self, new: usize) {
    self.buffers.pop_front();
    self.build(new);
  }

  fn iter(&self) -> impl Iterator<Item = &usize> {
    self.buffers.iter().flat_map(|i| i.1.iter())
  }
}

fn main() {
  let nums = stdin()
    .lines()
    .map(|line| line.unwrap().parse::<usize>().unwrap())
    .collect_vec();
  // let contents = lines.join("\n");

  let mut nums_iter = nums.iter();
  let mut sum_buffer = SumBuffer::default();
  for _ in 0..PREAMBLE_LENGTH {
    sum_buffer.build(*nums_iter.next().unwrap());
  }

  let target = loop {
    let num = nums_iter.next().unwrap();
    if !sum_buffer.iter().any(|sum| num == sum) {
      break num;
    }
    sum_buffer.next(*num);
  };

  println!("Part 1: {target}");

  let mut range = 0..2;
  let mut sum: usize = range.clone().map(|i| nums[i]).sum();
  let mut inch_forward = true;

  loop {
    // println!("{:?} ({}) w/ {}", range, inch_forward, sum);
    if inch_forward {
      if range.end == nums.len() {
        inch_forward = false;
        continue;
      }
      range.end += 1;
      sum += nums[range.end - 1];
    } else {
      if range.start == range.end - 2 {
        inch_forward = true;
        continue;
      }
      sum -= nums[range.start];
      range.start += 1;
    }

    //if sum < *target - nums[range.start] {
    //  expanse.start = range.start;
    //}
    //if sum > *target + nums[range.end - 1] {
    //  expanse.end = range.end;
    //  break false;
    //}

    if sum == *target {
      break;
    }

    inch_forward = sum < *target;
  }

  // great! we're done
  let MinMaxResult::MinMax(min, max) = range.map(|i| nums[i]).minmax() else {
    unreachable!()
  };

  println!("Part 2: {}", min + max);
}
