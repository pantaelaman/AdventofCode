use std::{collections::HashSet, io::stdin};

use itertools::Itertools;
use z3::{ast::Int, Optimize, SatResult};

#[derive(Debug)]
struct Machine {
  indicators: Vec<bool>,
  buttons: Vec<Vec<usize>>,
  joltages: Vec<usize>,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let machines = lines
    .iter()
    .map(|line| {
      let mut cs = line.chars();

      let Some('[') = cs.next() else {
        unimplemented!()
      };

      let mut indicators = Vec::new();

      while let Some(c) = cs.next() {
        match c {
          ']' => break,
          '.' => indicators.push(false),
          '#' => indicators.push(true),
          _ => unimplemented!(),
        }
      }

      let mut buttons = Vec::new();

      'buttons: loop {
        while let Some(c) = cs.next() {
          match c {
            '(' => break,
            '{' => break 'buttons,
            _ => continue,
          }
        }

        let mut button = Vec::new();
        let mut buf = String::new();

        while let Some(c) = cs.next() {
          match c {
            ')' => {
              button.push(buf.parse().unwrap());
              break;
            }
            ',' => {
              button.push(buf.parse().unwrap());
              buf.clear();
            }
            c => buf.push(c),
          }
        }

        buttons.push(button);
      }

      let mut joltages = Vec::new();
      let mut buf = String::new();

      while let Some(c) = cs.next() {
        match c {
          '}' => {
            joltages.push(buf.parse().unwrap());
            break;
          }
          ',' => {
            joltages.push(buf.parse().unwrap());
            buf.clear();
          }
          c => buf.push(c),
        }
      }

      Machine {
        indicators,
        buttons,
        joltages,
      }
    })
    .collect_vec();

  let presses = machines.iter().fold(0, |acc, machine| {
    let mut seen = HashSet::new();
    let mut frontier = vec![(0, vec![false; machine.indicators.len()])];

    loop {
      for (presses, state) in std::mem::take(&mut frontier) {
        if state == machine.indicators {
          return acc + presses;
        }

        if seen.contains(&state) {
          continue;
        }

        seen.insert(state.clone());

        frontier.extend(machine.buttons.iter().map(|button| {
          let mut next_state = state.clone();
          for i in button {
            next_state[*i] = !next_state[*i];
          }
          (presses + 1, next_state)
        }));
      }
    }
  });

  println!("Part 1: {presses}");

  let presses =
    machines
      .iter()
      .enumerate()
      .fold(0, |acc, (machine_idx, machine)| {
        let solver = Optimize::new();
        let button_consts = (0..machine.buttons.len())
          .map(|i| Int::new_const(i as i32))
          .collect_vec();
        for (j, joltage) in machine.joltages.iter().enumerate() {
          let equation = machine
            .buttons
            .iter()
            .enumerate()
            .filter_map(|(i, button)| {
              button.contains(&j).then_some(&button_consts[i])
            })
            .fold(Int::from_i64(0), std::ops::Add::add);

          solver.assert(&equation.eq(*joltage as i64));
        }

        for button_const in button_consts.iter() {
          solver.assert(&button_const.ge(0));
        }

        let sum = button_consts
          .into_iter()
          .reduce(std::ops::Add::add)
          .unwrap();
        solver.minimize(&sum);

        match solver.check(&[]) {
          SatResult::Sat => {
            let s = solver.get_model().unwrap().eval(&sum, true).unwrap();
            return acc + s.as_i64().unwrap();
          }
          _ => unimplemented!(),
        }
      });

  println!("Part 2: {presses}");
}
