#![feature(extend_one)]
use std::{
  collections::VecDeque,
  fs::File,
  io::{Read, Stdout, Write},
  ops::Index,
};

use itertools::Itertools;

pub trait Input {
  fn read(&mut self) -> i32;
}

pub struct StdinInput;

impl Input for StdinInput {
  fn read(&mut self) -> i32 {
    print!("input << ");
    std::io::stdout().flush().unwrap();
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().parse().unwrap()
  }
}

pub struct ProgrammedInput<I: Iterator<Item = i32>> {
  inputs: I,
}

impl<I: Iterator<Item = i32>> ProgrammedInput<I> {
  pub fn new<I2: IntoIterator<Item = i32, IntoIter = I>>(inputs: I2) -> Self {
    ProgrammedInput {
      inputs: inputs.into_iter(),
    }
  }
}

impl<I: Iterator<Item = i32>> Input for ProgrammedInput<I> {
  fn read(&mut self) -> i32 {
    self.inputs.next().unwrap()
  }
}

impl Input for VecDeque<i32> {
  fn read(&mut self) -> i32 {
    self.pop_front().unwrap()
  }
}

pub trait Output {
  fn write(&mut self, output: i32);
}

pub struct StdoutOutput;

impl Output for StdoutOutput {
  fn write(&mut self, output: i32) {
    println!("output >> {}", output);
  }
}

impl<O: Extend<i32>> Output for O {
  fn write(&mut self, output: i32) {
    self.extend_one(output);
  }
}

pub trait Context {
  fn input(&mut self) -> &mut dyn Input;
  fn output(&mut self) -> &mut dyn Output;
}

impl<C: Context> Context for &mut C {
  fn input(&mut self) -> &mut dyn Input {
    Context::input(*self)
  }

  fn output(&mut self) -> &mut dyn Output {
    Context::output(*self)
  }
}

pub struct OwnedContext<I: Input, O: Output> {
  input: I,
  output: O,
}

impl<I: Input, O: Output> OwnedContext<I, O> {
  fn new(input: I, output: O) -> Self {
    OwnedContext { input, output }
  }
}

impl<I: Input, O: Output> Context for OwnedContext<I, O> {
  fn input(&mut self) -> &mut dyn Input {
    &mut self.input
  }

  fn output(&mut self) -> &mut dyn Output {
    &mut self.output
  }
}

pub fn std_context() -> impl Context {
  OwnedContext {
    input: StdinInput,
    output: StdoutOutput,
  }
}

pub struct RefContext<'a> {
  input: &'a mut dyn Input,
  output: &'a mut dyn Output,
}

impl<'a> RefContext<'a> {
  pub fn new(input: &'a mut dyn Input, output: &'a mut dyn Output) -> Self {
    RefContext { input, output }
  }
}

impl Context for RefContext<'_> {
  fn input(&mut self) -> &mut dyn Input {
    self.input
  }

  fn output(&mut self) -> &mut dyn Output {
    self.output
  }
}

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
  Interrupt,
  Halt,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HaltingType {
  NoHalt,
  Interrupt,
  Final,
}

impl HaltingType {
  pub fn halted(self) -> bool {
    match self {
      Self::NoHalt => false,
      _ => true,
    }
  }
}

impl Command {
  pub fn apply(self, isp: &mut usize, program: &mut [i32]) -> HaltingType {
    match self {
      Command::Set { ptr, val } => program[ptr] = val,
      Command::Jump { ptr } => *isp = ptr,
      Command::Interrupt => return HaltingType::Interrupt,
      Command::Halt => return HaltingType::Final,
    }
    HaltingType::NoHalt
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
  fn operate(&self, args: Args<'_>, context: &mut dyn Context) -> Commands;
}

fn opcode_to_instr(opcode: usize) -> &'static dyn Instruction {
  match opcode % 100 {
    1 => &((&|a: i32, b: i32| a + b) as &BinaryOp),
    2 => &((&|a: i32, b: i32| a * b) as &BinaryOp),
    3 => &ReadInstr,
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

  fn operate(&self, args: Args<'_>, _context: &mut dyn Context) -> Commands {
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

  fn operate(&self, args: Args<'_>, _context: &mut dyn Context) -> Commands {
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

  fn operate(&self, args: Args<'_>, _context: &mut dyn Context) -> Commands {
    Command::Set {
      ptr: *args.immediate(2) as usize,
      val: self(args[0], args[1]) as i32,
    }
    .into()
  }
}

struct ReadInstr;

impl Instruction for ReadInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, context: &mut dyn Context) -> Commands {
    Command::Set {
      ptr: *args.immediate(0) as usize,
      val: context.input().read(),
    }
    .into()
  }
}

struct OutInstr;

impl Instruction for OutInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, context: &mut dyn Context) -> Commands {
    context.output().write(args[0]);
    Command::Interrupt.into()
  }
}

struct HaltInstr;

impl Instruction for HaltInstr {
  fn num_args(&self) -> usize {
    0
  }

  fn operate(&self, _args: Args<'_>, _context: &mut dyn Context) -> Commands {
    Command::Halt.into()
  }
}

pub struct Program<C: Context> {
  pub program: Vec<i32>,
  pub context: C,
  isp: usize,
  completed: bool,
}

impl<C: Context> Program<C> {
  pub fn new(program: Vec<i32>, context: C) -> Self {
    Program {
      program,
      context,
      isp: 0,
      completed: false,
    }
  }

  pub fn is_complete(&self) -> bool {
    self.completed
  }

  pub fn run_until_completed(&mut self) {
    while !self.completed {
      self.run_until_interrupt();
    }
  }

  pub fn run_until_interrupt(&mut self) {
    'prog: loop {
      let opcode = self.program[self.isp] as usize;
      let instr = opcode_to_instr(opcode);
      self.isp += 1;
      let num_args = instr.num_args();
      let args = &self.program[self.isp..self.isp + num_args];
      self.isp += num_args;
      // println!("{} at #{}", opcode, self.isp);
      for cmd in
        instr.operate(Args::new(args, &self.program, opcode), &mut self.context)
      {
        // println!("  {:?}", cmd);
        let halt = cmd.apply(&mut self.isp, &mut self.program);

        if halt == HaltingType::Final {
          self.completed = true;
        }

        if halt.halted() {
          break 'prog;
        }
      }
    }
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

  let out = (0..5)
    .permutations(5)
    .map(|i| {
      i.into_iter().fold(0, |acc, shift| {
        let mut output = Vec::new();
        let mut input = ProgrammedInput::new([shift, acc]);
        let mut program = Program::new(
          program.clone(),
          RefContext::new(&mut input, &mut output),
        );
        program.run_until_completed();
        output[0]
      })
    })
    .max()
    .unwrap();

  println!("Part 1: {}", out);

  let out = (5..10)
    .permutations(5)
    .map(|i| {
      let mut amplifier_programs = Vec::new();

      for shift in i {
        let mut context = OwnedContext {
          input: VecDeque::new(),
          output: Vec::new(),
        };
        context.input.push_back(shift);
        let local_program = Program::new(program.clone(), context);
        amplifier_programs.push(local_program);
      }

      let mut acc = 0;
      loop {
        let mut breaking = false;
        for program in amplifier_programs.iter_mut() {
          program.context.input.push_back(acc);
          program.run_until_interrupt();
          acc = *program.context.output.last().unwrap();
          if program.is_complete() {
            breaking = true;
          }
        }
        if breaking {
          break;
        }
      }
      acc
    })
    .max()
    .unwrap();

  println!("Part 2: {}", out);
}
