use std::{io::stdin, str::FromStr};

use itertools::Itertools;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Instruction {
  Hlf(usize),
  Tpl(usize),
  Inc(usize),
  Jmp(isize),
  Jie(usize, isize),
  Jio(usize, isize),
}

impl FromStr for Instruction {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let (instr, tail) = s.split_once(" ").unwrap();
    let mut tail = tail.split(", ");

    fn str_to_reg(s: &str) -> u8 {
      s.chars().nth(0).unwrap() as u8 - b'a'
    }

    Ok(match instr {
      "hlf" => Self::Hlf(str_to_reg(tail.next().unwrap()) as usize),
      "tpl" => Self::Tpl(str_to_reg(tail.next().unwrap()) as usize),
      "inc" => Self::Inc(str_to_reg(tail.next().unwrap()) as usize),
      "jmp" => Self::Jmp(tail.next().unwrap().parse().unwrap()),
      "jie" => Self::Jie(
        str_to_reg(tail.next().unwrap()) as usize,
        tail.next().unwrap().parse().unwrap(),
      ),
      "jio" => Self::Jio(
        str_to_reg(tail.next().unwrap()) as usize,
        tail.next().unwrap().parse().unwrap(),
      ),
      _ => return Err(()),
    })
  }
}

impl Instruction {
  fn run(&self, regs: &mut [i32]) -> isize {
    match self {
      Self::Hlf(reg) => regs[*reg] /= 2,
      Self::Tpl(reg) => regs[*reg] *= 3,
      Self::Inc(reg) => regs[*reg] += 1,
      Self::Jmp(offset) => return *offset,
      Self::Jie(reg, offset) => {
        if regs[*reg] % 2 == 0 {
          return *offset;
        }
      }
      Self::Jio(reg, offset) => {
        if regs[*reg] == 1 {
          return *offset;
        }
      }
    }
    1
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let instructions: Vec<Instruction> =
    lines.iter().map(|line| line.parse().unwrap()).collect_vec();

  let mut isp = 0;
  let mut regs = [0; 2];
  while let Some(instr) = instructions.get(isp) {
    isp = isp.checked_add_signed(instr.run(&mut regs)).unwrap();
  }
  println!("Part 1: {}", regs[1]);

  let mut isp = 0;
  let mut regs = [1, 0];
  while let Some(instr) = instructions.get(isp) {
    isp = isp.checked_add_signed(instr.run(&mut regs)).unwrap();
  }
  println!("Part 2: {}", regs[1]);
}
