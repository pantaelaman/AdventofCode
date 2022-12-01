use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
  let max_calories = process_input("input.txt");
  println!("Part 1: {}", max_calories.0);
  println!("Part 2: {}", max_calories.0 + max_calories.1 + max_calories.2);
}

fn process_input(file: &str) -> (i32, i32, i32) {
  let file = File::open(file).expect("invalid path");
  let reader = BufReader::new(file);

  let mut max_calories = (0, 0, 0);
  let mut current_total = 0;

  for (index,line) in reader.lines().enumerate() {
    let line = line.expect(format!("could not read line {}", index).as_str());
    if line == "" {
      update_calories(&mut current_total, &mut max_calories);
    } else {
      current_total += i32::from_str_radix(&line, 10).expect(format!("invalid format on line {}", line).as_str());
    }
  }

  update_calories(&mut current_total, &mut max_calories);

  max_calories
}

fn update_calories(current_total: &mut i32, max_calories: &mut (i32, i32, i32)) {
  if *current_total > max_calories.0 {
    max_calories.2 = max_calories.1;
    max_calories.1 = max_calories.0;
    max_calories.0 = *current_total;
  } else if *current_total > max_calories.1 {
    max_calories.2 = max_calories.1;
    max_calories.1 = *current_total;
  } else if *current_total > max_calories.2 {
    max_calories.2 = *current_total;
  }
  *current_total = 0;
}

