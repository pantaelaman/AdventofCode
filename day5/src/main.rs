use std::{
  collections::VecDeque,
  fs::File,
  io::{Read, Write},
  ops::Index,
};

use itertools::Itertools;

pub struct Args<'a> {
  raw: &'a [i32],
  program: &'a [i32],
  modes: usize,
}

impl<'a> Args<'a> {
  pub fn new(raw: &'a [i32], program: &'a [i32], opcode: usize) -> Self {
    Args {
      raw,
      program,
      modes: opcode / 100,
    }
  }

  pub fn position(&self, index: usize) -> &i32 {
    &self.program[self.raw[index] as usize]
  }

  pub fn immediate(&self, index: usize) -> &i32 {
    &self.raw[index]
  }
}

impl Index<usize> for Args<'_> {
  type Output = i32;

  fn index(&self, index: usize) -> &Self::Output {
    let mode = (self.modes / 10usize.pow(index as u32)) % 10;
    let value = match mode {
      0 => &self.program[self.raw[index] as usize],
      1 => &self.raw[index],
      _ => unreachable!(),
    };
    value
  }
}

#[derive(Debug)]
pub enum Command {
  Set { ptr: usize, val: i32 },
  Jump { ptr: usize },
  Halt,
}

impl Command {
  pub fn apply(self, isp: &mut usize, program: &mut [i32]) -> bool {
    match self {
      Command::Set { ptr, val } => program[ptr] = val,
      Command::Jump { ptr } => *isp = ptr,
      Command::Halt => return true,
    }
    false
  }

  pub fn chain<T: Into<Commands>>(self, next: T) -> Commands {
    Commands::hint_multi(self).chain(next)
  }
}

#[derive(Default)]
pub enum Commands {
  #[default]
  Empty,
  Single(Command),
  Multi(VecDeque<Command>),
}

impl Commands {
  fn hint_multi(command: Command) -> Self {
    let mut v = VecDeque::new();
    v.push_back(command);
    Self::Multi(v)
  }

  pub fn chain<T: Into<Commands>>(self, next: T) -> Commands {
    match self {
      Self::Empty => next.into(),
      Self::Single(cmd) => {
        let mut cmds = VecDeque::new();
        cmds.push_back(cmd);
        next.into().drain(&mut cmds);
        Commands::Multi(cmds)
      }
      Self::Multi(mut cmds) => {
        next.into().drain(&mut cmds);
        Commands::Multi(cmds)
      }
    }
  }

  fn drain<T: Extend<Command>>(self, target: &mut T) {
    match self {
      Self::Empty => {}
      Self::Single(cmd) => target.extend(std::iter::once(cmd)),
      Self::Multi(cmds) => target.extend(cmds),
    }
  }
}

impl Iterator for Commands {
  type Item = Command;

  fn next(&mut self) -> Option<Self::Item> {
    match std::mem::take(self) {
      Self::Empty => None,
      Self::Single(cmd) => {
        *self = Self::Empty;
        Some(cmd)
      }
      Self::Multi(mut cmds) => {
        let result = cmds.pop_front();
        *self = Self::Multi(cmds);
        result
      }
    }
  }
}

impl Into<Commands> for Command {
  fn into(self) -> Commands {
    Commands::Single(self)
  }
}

trait Instruction {
  fn num_args(&self) -> usize;
  fn operate(&self, args: Args<'_>, program: &[i32]) -> Commands;
}

fn opcode_to_instr(opcode: usize) -> &'static dyn Instruction {
  match opcode % 100 {
    1 => &((&|a: i32, b: i32| a + b) as &BinaryOp),
    2 => &((&|a: i32, b: i32| a * b) as &BinaryOp),
    3 => &SetInstr,
    4 => &OutInstr,
    5 => &((&|v: i32| v != 0) as &TestOp),
    6 => &((&|v: i32| v == 0) as &TestOp),
    7 => &((&|a: i32, b: i32| a < b) as &CmpOp),
    8 => &((&|a: i32, b: i32| a == b) as &CmpOp),
    99 => &HaltInstr,
    _ => unreachable!(),
  }
}

type BinaryOp = dyn Fn(i32, i32) -> i32;
type TestOp = dyn Fn(i32) -> bool;
type CmpOp = dyn Fn(i32, i32) -> bool;

impl Instruction for &BinaryOp {
  fn num_args(&self) -> usize {
    3
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    Command::Set {
      ptr: *args.immediate(2) as usize,
      val: self(args[0], args[1]),
    }
    .into()
  }
}

impl Instruction for &TestOp {
  fn num_args(&self) -> usize {
    2
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    if self(args[0]) {
      Command::Jump {
        ptr: args[1] as usize,
      }
      .into()
    } else {
      Commands::Empty
    }
  }
}

impl Instruction for &CmpOp {
  fn num_args(&self) -> usize {
    3
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    Command::Set {
      ptr: *args.immediate(2) as usize,
      val: self(args[0], args[1]) as i32,
    }
    .into()
  }
}

struct SetInstr;

impl Instruction for SetInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    print!("input << ");
    std::io::stdout().flush().unwrap();
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).unwrap();
    let output = buffer.trim().parse::<i32>().unwrap();
    Command::Set {
      ptr: *args.immediate(0) as usize,
      val: output,
    }
    .into()
  }
}

struct OutInstr;

impl Instruction for OutInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    println!("output >> {}", args[0]);
    Commands::Empty
  }
}

struct JZRInstr;

impl Instruction for JZRInstr {
  fn num_args(&self) -> usize {
    2
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    if args[0] == 0 {
      Command::Jump {
        ptr: args[1] as usize,
      }
      .into()
    } else {
      Commands::Empty
    }
  }
}

struct JNZInstr;

impl Instruction for JNZInstr {
  fn num_args(&self) -> usize {
    2
  }

  fn operate(&self, args: Args<'_>, _program: &[i32]) -> Commands {
    if args[0] != 0 {
      Command::Jump {
        ptr: args[1] as usize,
      }
      .into()
    } else {
      Commands::Empty
    }
  }
}

struct HaltInstr;

impl Instruction for HaltInstr {
  fn num_args(&self) -> usize {
    0
  }

  fn operate(&self, _args: Args<'_>, _program: &[i32]) -> Commands {
    Command::Halt.into()
  }
}

fn main() {
  let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let contents = {
    let mut target = String::new();
    file.read_to_string(&mut target).unwrap();
    target
  };

  let program = contents
    .split(',')
    .map(|v| v.trim().parse::<i32>().unwrap())
    .collect_vec();

  let final_state = run_program(program);

  println!("{:?}", final_state);
}

fn run_program(mut program: Vec<i32>) -> Vec<i32> {
  let mut isp = 0;
  'prog: loop {
    let opcode = program[isp] as usize;
    let instr = opcode_to_instr(opcode);
    isp += 1;
    let num_args = instr.num_args();
    let args = &program[isp..isp + num_args];
    isp += num_args;
    println!("{} at #{}", opcode, isp);
    for cmd in instr.operate(Args::new(args, &program, opcode), &program) {
      println!("  {:?}", cmd);
      if cmd.apply(&mut isp, &mut program) {
        break 'prog;
      }
    }
  }
  program
}
