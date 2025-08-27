use std::{collections::HashMap, io::stdin, str::FromStr};

use either::Either;
use itertools::Itertools;
use regex::Regex;

#[derive(Debug, Clone, Copy, Default)]
struct Mask {
  affects: u64,
  mask: u64,
}

/// affects:mask
/// 0:0 = untouched
/// 0:1 = fluctuating
/// 1:0 = definite 0
/// 1:1 = definite 1
#[derive(Debug, Clone, Copy, Default)]
struct FluctMask {
  affects: u64,
  mask: u64,
}

impl FromStr for FluctMask {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(s.chars().fold(FluctMask::default(), |mut mask, c| {
      mask = FluctMask {
        affects: mask.affects << 1,
        mask: mask.mask << 1,
      };
      match c {
        '1' => {
          mask.affects |= 1;
          mask.mask |= 1;
        }
        'X' => {
          mask.mask |= 1;
        }
        _ => {}
      }
      mask
    }))
  }
}

impl FluctMask {
  fn gen_masks(&self) -> Vec<Mask> {
    let mut head = vec![*self];
    let mut collection = Vec::new();
    while !head.is_empty() {
      for thing in std::mem::take(&mut head) {
        if (!thing.affects & thing.mask).count_ones() == 0 {
          // no more fluctuations, push this in
          collection.push(Mask {
            affects: thing.affects,
            mask: thing.mask,
          });
          continue;
        }

        let first_fluct = (!thing.affects & thing.mask).ilog2();
        let new_affects = thing.affects | 1 << first_fluct;

        head.push(FluctMask {
          affects: new_affects,
          mask: thing.mask, // 1:1
        });
        head.push(FluctMask {
          affects: new_affects,
          mask: thing.mask & !(1 << first_fluct), // 1:0
        });
      }
    }

    collection
  }
}

impl FromStr for Mask {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(s.chars().fold(Mask::default(), |mut mask, c| {
      mask = Mask {
        affects: mask.affects << 1,
        mask: mask.mask << 1,
      };
      match c {
        '1' => {
          mask.affects |= 1;
          mask.mask |= 1;
        }
        '0' => {
          mask.affects |= 1;
        }
        _ => {}
      }
      mask
    }))
  }
}

impl Mask {
  fn apply(self, value: u64) -> u64 {
    (self.affects & self.mask) | (!self.affects & value)
  }
}

#[derive(Debug, Clone, Copy, Default)]
struct MemWrite {
  address: usize,
  value: u64,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let mask_re = Regex::new(r"mask = ([X01]{36})").unwrap();
  let mem_re = Regex::new(r"mem\[(\d+)\] = (\d+)").unwrap();

  let instrs: Vec<Either<Mask, MemWrite>> = lines
    .iter()
    .map(|line| {
      mask_re
        .captures(&line)
        .map(|caps| Either::Left(caps[1].parse::<Mask>().unwrap()))
        .unwrap_or_else(|| {
          let caps = mem_re.captures(&line).unwrap();
          Either::Right(MemWrite {
            address: caps[1].parse().unwrap(),
            value: caps[2].parse().unwrap(),
          })
        })
    })
    .collect_vec();

  let (_, mem): (_, HashMap<usize, u64>) = instrs.iter().fold(
    (Mask::default(), HashMap::new()),
    |(mask, mut mem), instr| match instr {
      Either::Left(mask) => (*mask, mem),
      Either::Right(MemWrite { address, value }) => {
        mem.insert(*address, mask.apply(*value));

        (mask, mem)
      }
    },
  );

  println!("Part 1: {}", mem.values().sum::<u64>());

  let instrs: Vec<Either<FluctMask, MemWrite>> = lines
    .iter()
    .map(|line| {
      mask_re
        .captures(&line)
        .map(|caps| Either::Left(caps[1].parse::<FluctMask>().unwrap()))
        .unwrap_or_else(|| {
          let caps = mem_re.captures(&line).unwrap();
          Either::Right(MemWrite {
            address: caps[1].parse().unwrap(),
            value: caps[2].parse().unwrap(),
          })
        })
    })
    .collect_vec();

  let (_, mem): (_, HashMap<usize, u64>) = instrs.iter().fold(
    (Vec::<Mask>::new(), HashMap::new()),
    |(masks, mut mem), instr| match instr {
      Either::Left(mask) => {
        let new_masks = mask.gen_masks();
        (new_masks, mem)
      }
      Either::Right(MemWrite { address, value }) => {
        for mask in masks.iter() {
          mem.insert(mask.apply(*address as u64) as usize, *value);
        }
        (masks, mem)
      }
    },
  );

  println!("Part 2: {}", mem.values().sum::<u64>());
}
