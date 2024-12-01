use std::{collections::VecDeque, fs::File, io::Read};

use itertools::Itertools;

#[derive(Debug)]
pub enum Command {
  Set { ptr: usize, val: usize },
  Halt,
}

impl Command {
  pub fn apply(self, program: &mut [usize]) -> bool {
    match self {
      Command::Set { ptr, val } => {
        program[ptr] = val;
        false
      }
      Command::Halt => true,
    }
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
  fn operate(&self, args: &[usize], program: &[usize]) -> Commands;
}

fn opcode_to_instr(opcode: usize) -> &'static dyn Instruction {
  match opcode {
    1 => &AddInstr,
    2 => &MulInstr,
    99 => &HaltInstr,
    _ => unreachable!(),
  }
}

trait BinaryOp {
  fn operate(&self, inp1: usize, inp2: usize) -> usize;
}

impl<T: BinaryOp> Instruction for T {
  fn num_args(&self) -> usize {
    3
  }

  fn operate(&self, args: &[usize], program: &[usize]) -> Commands {
    Command::Set {
      ptr: args[2],
      val: self.operate(program[args[0]], program[args[1]]),
    }
    .into()
  }
}

struct AddInstr;

impl BinaryOp for AddInstr {
  fn operate(&self, inp1: usize, inp2: usize) -> usize {
    inp1 + inp2
  }
}

struct MulInstr;

impl BinaryOp for MulInstr {
  fn operate(&self, inp1: usize, inp2: usize) -> usize {
    inp1 * inp2
  }
}

struct HaltInstr;

impl Instruction for HaltInstr {
  fn num_args(&self) -> usize {
    0
  }

  fn operate(&self, _args: &[usize], _program: &[usize]) -> Commands {
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
    .map(|v| v.trim().parse::<usize>().unwrap())
    .collect_vec();

  for noun in 0..100 {
    for verb in 0..100 {
      let mut cloned = program.clone();
      cloned[1] = noun;
      cloned[2] = verb;
      let final_state = run_program(cloned);
      if noun == 12 && verb == 2 {
        println!("Part 1: {}", final_state[0]);
      }
      if final_state[0] == 19690720 {
        println!("Part 2: {}", 100 * noun + verb);
      }
    }
  }
}

fn run_program(mut program: Vec<usize>) -> Vec<usize> {
  let mut isp = 0;
  'prog: loop {
    let instr = opcode_to_instr(program[isp]);
    isp += 1;
    let num_args = instr.num_args();
    let args = &program[isp..isp + num_args];
    isp += num_args;
    for cmd in instr.operate(args, &program) {
      if cmd.apply(&mut program) {
        break 'prog;
      }
    }
  }
  program
}
