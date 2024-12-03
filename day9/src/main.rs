#![feature(strict_overflow_ops)]
#![feature(extend_one)]
use std::{
  collections::{HashMap, VecDeque},
  fs::File,
  io::{Read, Stdout, Write},
  ops::{Index, IndexMut},
};

use itertools::Itertools;

type Value = i128;
type RawProgram = HashMap<usize, Value>;

pub struct Context<'a> {
  pub io: &'a mut dyn IOPipeline,
  pub args: Args<'a>,
}

pub trait Input {
  fn read(&mut self) -> Value;
}

pub struct StdinInput;

impl Input for StdinInput {
  fn read(&mut self) -> Value {
    print!("input << ");
    std::io::stdout().flush().unwrap();
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().parse().unwrap()
  }
}

pub struct ProgrammedInput<I: Iterator<Item = Value>> {
  inputs: I,
}

impl<I: Iterator<Item = Value>> ProgrammedInput<I> {
  pub fn new<I2: IntoIterator<Item = Value, IntoIter = I>>(inputs: I2) -> Self {
    ProgrammedInput {
      inputs: inputs.into_iter(),
    }
  }
}

impl<I: Iterator<Item = Value>> Input for ProgrammedInput<I> {
  fn read(&mut self) -> Value {
    self.inputs.next().unwrap()
  }
}

impl Input for VecDeque<Value> {
  fn read(&mut self) -> Value {
    self.pop_front().unwrap()
  }
}

pub trait Output {
  fn write(&mut self, output: Value);
}

pub struct StdoutOutput;

impl Output for StdoutOutput {
  fn write(&mut self, output: Value) {
    println!("output >> {}", output);
  }
}

impl<O: Extend<Value>> Output for O {
  fn write(&mut self, output: Value) {
    self.extend_one(output);
  }
}

pub trait IOPipeline {
  fn input(&mut self) -> &mut dyn Input;
  fn output(&mut self) -> &mut dyn Output;
}

impl<IO: IOPipeline> IOPipeline for &mut IO {
  fn input(&mut self) -> &mut dyn Input {
    IOPipeline::input(*self)
  }

  fn output(&mut self) -> &mut dyn Output {
    IOPipeline::output(*self)
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

impl<I: Input, O: Output> IOPipeline for OwnedContext<I, O> {
  fn input(&mut self) -> &mut dyn Input {
    &mut self.input
  }

  fn output(&mut self) -> &mut dyn Output {
    &mut self.output
  }
}

pub fn std_context() -> impl IOPipeline {
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

impl IOPipeline for RefContext<'_> {
  fn input(&mut self) -> &mut dyn Input {
    self.input
  }

  fn output(&mut self) -> &mut dyn Output {
    self.output
  }
}

pub struct Args<'a> {
  num_args: usize,
  ptr: usize,
  program: &'a RawProgram,
  rsp: &'a usize,
  modes: usize,
}

impl<'a> Args<'a> {
  pub fn new(
    num_args: usize,
    ptr: usize,
    program: &'a RawProgram,
    rsp: &'a usize,
    opcode: usize,
  ) -> Self {
    Args {
      num_args,
      ptr,
      program: program,
      rsp: &rsp,
      modes: opcode / 100,
    }
  }

  pub fn write(&self, index: usize, val: Value) -> Command {
    Command::Set {
      ptr: match self.get_mode(index) {
        0 | 1 => *self.immediate(index) as usize,
        2 => self.rsp.strict_add_signed(*self.immediate(index) as isize),
        _ => unreachable!(),
      },
      val: val,
    }
  }

  fn get_mode(&self, index: usize) -> usize {
    (self.modes / 10usize.pow(index as u32)) % 10
  }

  pub fn position(&self, index: usize) -> &Value {
    self.program.deref_ptr(*self.immediate(index) as usize)
  }

  pub fn immediate(&self, index: usize) -> &Value {
    self.program.deref_ptr(self.ptr + index)
  }

  pub fn relative(&self, index: usize) -> &Value {
    //    println!("Relative access >>>");
    //    println!(
    //      "Relative access: {} + {} = {}",
    //      self.rsp,
    //      self.program.deref_ptr(self.ptr + index),
    //      self.program.deref_ptr(
    //        self
    //          .rsp
    //          .strict_add_signed(*self.program.deref_ptr(self.ptr + index) as isize)
    //      )
    //    );
    self
      .program
      .deref_ptr(self.rsp.strict_add_signed(*self.immediate(index) as isize))
  }
}

impl Index<usize> for Args<'_> {
  type Output = Value;

  fn index(&self, index: usize) -> &Self::Output {
    let value = match self.get_mode(index) {
      0 => self.position(index),
      1 => self.immediate(index),
      2 => self.relative(index),
      _ => unreachable!(),
    };
    value
  }
}

#[derive(Debug)]
pub enum Command {
  Set { ptr: usize, val: Value },
  AdjustRsp { by: i128 },
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
  pub fn apply(
    self,
    isp: &mut usize,
    program: &mut RawProgram,
    rsp: &mut usize,
  ) -> HaltingType {
    match self {
      Command::Set { ptr, val } => _ = program.insert(ptr, val),
      Command::AdjustRsp { by } => *rsp = rsp.strict_add_signed(by as isize),
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
  fn operate(&self, args: Args<'_>, context: &mut dyn IOPipeline) -> Commands;
}

fn log_harness(
  opcode: usize,
  instruction: &dyn Instruction,
  args: Args<'_>,
  context: &mut dyn IOPipeline,
) -> Commands {
  println!(
    "{} ({}) ({:?})",
    match opcode % 100 {
      1 => "add",
      2 => "mul",
      3 => "inp",
      4 => "out",
      5 => "jnz",
      6 => "jze",
      7 => "lth",
      8 => "equ",
      9 => "rsp",
      99 => "halt",
      instr => unreachable!("{instr}"),
    },
    opcode,
    (0..instruction.num_args())
      .map(|i| (args.immediate(i), args[i]))
      .collect_vec()
  );

  instruction.operate(args, context)
}

fn opcode_to_instr(opcode: usize) -> &'static dyn Instruction {
  match opcode % 100 {
    1 => &((&|a: Value, b: Value| a + b) as &BinaryOp),
    2 => &((&|a: Value, b: Value| a * b) as &BinaryOp),
    3 => &ReadInstr,
    4 => &OutInstr,
    5 => &((&|v: Value| v != 0) as &TestOp),
    6 => &((&|v: Value| v == 0) as &TestOp),
    7 => &((&|a: Value, b: Value| a < b) as &CmpOp),
    8 => &((&|a: Value, b: Value| a == b) as &CmpOp),
    9 => &SetRspInstr,
    99 => &HaltInstr,
    instr => unreachable!("{instr}"),
  }
}

type BinaryOp = dyn Fn(Value, Value) -> Value;
type TestOp = dyn Fn(Value) -> bool;
type CmpOp = dyn Fn(Value, Value) -> bool;

impl Instruction for &BinaryOp {
  fn num_args(&self) -> usize {
    3
  }

  fn operate(&self, args: Args<'_>, _context: &mut dyn IOPipeline) -> Commands {
    args.write(2, self(args[0], args[1])).into()
  }
}

impl Instruction for &TestOp {
  fn num_args(&self) -> usize {
    2
  }

  fn operate(&self, args: Args<'_>, _context: &mut dyn IOPipeline) -> Commands {
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

  fn operate(&self, args: Args<'_>, _context: &mut dyn IOPipeline) -> Commands {
    args.write(2, self(args[0], args[1]) as Value).into()
  }
}

struct ReadInstr;

impl Instruction for ReadInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, context: &mut dyn IOPipeline) -> Commands {
    args.write(0, context.input().read()).into()
  }
}

struct OutInstr;

impl Instruction for OutInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, context: &mut dyn IOPipeline) -> Commands {
    context.output().write(args[0]);
    Command::Interrupt.into()
  }
}

struct HaltInstr;

impl Instruction for HaltInstr {
  fn num_args(&self) -> usize {
    0
  }

  fn operate(
    &self,
    _args: Args<'_>,
    _context: &mut dyn IOPipeline,
  ) -> Commands {
    Command::Halt.into()
  }
}

struct SetRspInstr;

impl Instruction for SetRspInstr {
  fn num_args(&self) -> usize {
    1
  }

  fn operate(&self, args: Args<'_>, _context: &mut dyn IOPipeline) -> Commands {
    Command::AdjustRsp { by: args[0] }.into()
  }
}

trait ProgramAccess {
  fn deref_ptr(&self, ptr: usize) -> &i128;
}

impl ProgramAccess for RawProgram {
  fn deref_ptr(&self, ptr: usize) -> &i128 {
    // println!("dereffing {ptr} to {:?}", self.get(&ptr));
    self.get(&ptr).unwrap_or(&0)
  }
}

impl Index<usize> for &dyn ProgramAccess {
  type Output = i128;

  fn index(&self, index: usize) -> &Self::Output {
    self.deref_ptr(index)
  }
}

pub struct Program<C: IOPipeline> {
  pub program: RawProgram,
  pub context: C,
  isp: usize,
  rsp: usize,
  completed: bool,
}

impl<C: IOPipeline> Program<C> {
  pub fn new(program: impl IntoIterator<Item = Value>, context: C) -> Self {
    Program {
      program: program.into_iter().enumerate().collect(),
      context,
      isp: 0,
      rsp: 0,
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
      let opcode = self.program[&self.isp] as usize;
      let instr = opcode_to_instr(opcode);
      let num_args = instr.num_args();
      self.isp += 1 + num_args;
      let args = Args::new(
        num_args,
        self.isp - num_args,
        &self.program,
        &self.rsp,
        opcode,
      );
      //for cmd in log_harness(opcode, instr, args, &mut self.context) {
      for cmd in instr.operate(args, &mut self.context) {
        let halt = cmd.apply(&mut self.isp, &mut self.program, &mut self.rsp);

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

  let raw_program = contents
    .split(',')
    .map(|v| v.trim().parse::<Value>().unwrap())
    .collect_vec();

  let mut program = Program::new(raw_program, std_context());
  program.run_until_completed();
}
