use std::io::stdin;

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let ranges = lines[0]
    .split(',')
    .map(|range| {
      let (lower, upper) = range
        .split('-')
        .map(|num| num.parse::<usize>().unwrap())
        .collect_tuple()
        .unwrap();
      lower..=upper
    })
    .collect_vec();

  let num_invalid = ranges
    .iter()
    .flat_map(|range| range.clone())
    .filter(|n| {
      let width = n.ilog10() + 1;
      if width % 2 != 0 {
        return false;
      }

      let stringed = format!("{n}");
      let (front, back) = stringed.split_at(width as usize / 2);
      front == back
    })
    .sum::<usize>();

  println!("Part 1: {}", num_invalid);

  let num_invalid = ranges
    .iter()
    .flat_map(|range| range.clone())
    .filter(|n| {
      let stringed = format!("{n}").into_bytes();

      'search: for i in 1..=(stringed.len() / 2) {
        if stringed.len() % i != 0 {
          continue;
        }

        for j in (i..stringed.len()).step_by(i) {
          if stringed[0..i] != stringed[j..j + i] {
            continue 'search;
          }
        }

        return true;
      }

      false
    })
    .sum::<usize>();

  println!("Part 2: {}", num_invalid);
}
