use std::fs::File;
use std::io::{BufReader, BufRead};

struct Instruction {
  cycle_amount: i32,
  state: i32,
  runner: &'static dyn Fn(&i32, &mut i32), // (register)
}

fn addx(state: &i32, register: &mut i32) {
  *register += state;
}

fn noop(_: &i32, _: &mut i32) {}

fn read_instructions(filename: &str) -> Vec<Instruction> {
  let file = File::open(filename).unwrap();
  let reader = BufReader::new(file);

  let mut instructions = Vec::new();
  for line in reader.lines() {
    let parts = line.unwrap().split(" ").map(|e| e.to_string()).collect::<Vec<String>>();
    
    instructions.push(match parts[0].as_str() {
      "addx" => Instruction {
        cycle_amount: 2,
        state: i32::from_str_radix(parts[1].as_str(), 10).unwrap(),
        runner: &addx,
      },
      "noop" => Instruction {
        cycle_amount: 1,
        state: 0,
        runner: &noop,
      },
      _ => unimplemented!(),
    });
  }

  instructions
}

fn run_instructions(instructions: &Vec<Instruction>) -> (i32, String) {
  let mut register: i32 = 1;
  let mut display = vec![vec!['.'; 40]; 6];
  let mut instructions = instructions.iter();
  let mut current_instruction: &Instruction = instructions.next().unwrap(); // assume length > 1
  let mut current_cycle = 0; // synced globally
  let mut working_cycle = 0; // synced to instruction
  let mut signal_sum = 0;
  loop {
    if (current_cycle + 1) % 40 == 20 && current_cycle < 220 {
      signal_sum += register * (current_cycle + 1);
    }

    for i in (register - 1)..(register + 2) {
      if i == current_cycle % 40 {
        display[(current_cycle / 40) as usize][(current_cycle % 40) as usize] = '\u{2588}';
        break
      }
    }

    if working_cycle + 1 == current_instruction.cycle_amount {
      (current_instruction.runner)(&current_instruction.state, &mut register);
      current_instruction = match instructions.next() {
        Some(v) => v,
        None => break,
      };
      working_cycle = 0;
    } else {
      working_cycle += 1;
    }

    current_cycle += 1;
  }
  (signal_sum, display.iter().fold("".to_string(), |a, e| a + "\n" + &e.iter().collect::<String>()))
}

fn main() {
  let instructions = read_instructions("input.txt");
  let output = run_instructions(&instructions);
  println!("Part 1: {}", output.0);
  println!("Part 2:");
  println!("{}", output.1);
}
