use std::io::stdin;

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let target: usize = lines[0].parse().unwrap();
  let size = target / 10;
  let mut houses = vec![0; size];
  for elf in 0..size {
    for house in (elf..size).step_by(elf + 1) {
      houses[house] += (elf + 1) * 10;
    }
  }
  let house_num = houses.iter().position(|v| v >= &target).unwrap();
  println!("Part 1: {}", house_num + 1);

  let mut houses = vec![0; size];
  for elf in 0..size {
    for house in (elf..size).step_by(elf + 1).take(50) {
      houses[house] += (elf + 1) * 11;
    }
  }
  let house_num = houses.iter().position(|v| v >= &target).unwrap();
  println!("Part 2: {}", house_num + 1);
}
