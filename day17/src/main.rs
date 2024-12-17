use std::{
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");

  let regex = Regex::new(r"(\d+)").unwrap();

  let mut nums = regex
    .find_iter(&contents)
    .map(|s| s.as_str().parse::<usize>().unwrap());

  let debug_a = nums.next().unwrap();
  let program = nums.skip(2).collect_vec();

  let output = run_prog(debug_a, &program);

  let str_out = output.into_iter().map(|n| n.to_string()).join(",");
  println!("Part 1: {str_out}");

  let running_total = dfs_a(0, &program, program.clone()).unwrap();
  println!("Part 2: {running_total}");
}

fn dfs_a(
  basis: usize,
  program: &Vec<usize>,
  mut eatable: Vec<usize>,
) -> Option<usize> {
  let val = match eatable.pop() {
    Some(v) => v,
    None => return Some(basis),
  };
  (0..8)
    .map(|a| (basis << 3) + a)
    .filter(|a| run_prog(*a, &program)[0] == val)
    .filter_map(|a| dfs_a(a, program, eatable.clone()))
    .next()
}

fn run_prog(mut a: usize, program: &Vec<usize>) -> Vec<usize> {
  let mut output = Vec::new();
  let mut b = 0;
  let mut c = 0;

  let mut isp = 0;
  loop {
    let instr = match program.get(isp) {
      Some(v) => *v,
      None => break,
    };
    let operand = match program.get(isp + 1) {
      Some(v) => *v,
      None => break,
    };
    let combo_op_val = match operand {
      4 => a,
      5 => b,
      6 => c,
      7 => unreachable!(),
      v => v,
    };

    match instr {
      0 => {
        a >>= combo_op_val;
      }
      1 => {
        b = b ^ operand;
      }
      2 => {
        b = combo_op_val % 8;
      }
      3 => {
        if a != 0 {
          isp = operand;
          continue;
        }
      }
      4 => {
        b ^= c;
      }
      5 => {
        output.push(combo_op_val % 8);
      }
      6 => {
        b = a >> combo_op_val;
      }
      7 => {
        c = a >> combo_op_val;
      }
      _ => unimplemented!(),
    }

    isp = isp + 2;
  }

  output
}
