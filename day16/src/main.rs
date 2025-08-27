use std::{
  collections::{HashMap, HashSet},
  io::stdin,
  ops::RangeInclusive,
};

use itertools::Itertools;
use regex::Regex;

type Ticket = Vec<usize>;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");
  let (defs, you, nearby) = contents.split("\n\n").collect_tuple().unwrap();
  let def_re = Regex::new(r"([\w\s]+): (\d+)\-(\d+) or (\d+)\-(\d+)").unwrap();
  let defs: HashMap<String, [RangeInclusive<usize>; 2]> = defs
    .lines()
    .map(|line| {
      let caps = def_re.captures(line).unwrap();
      let name = caps[1].to_string();
      let (r00, r01, r10, r11) = caps
        .iter()
        .skip(2)
        .map(|s| s.unwrap().as_str().parse::<usize>().unwrap())
        .collect_tuple()
        .unwrap();

      (name, [r00..=r01, r10..=r11])
    })
    .collect();

  let you = you
    .lines()
    .nth(1)
    .unwrap()
    .split(',')
    .map(|n| n.parse::<usize>().unwrap())
    .collect_vec();

  let nearby = nearby
    .lines()
    .skip(1)
    .map(|line| {
      line
        .split(',')
        .map(|n| n.parse::<usize>().unwrap())
        .collect_vec()
    })
    .collect_vec();

  let mut error_rate = 0;
  let mut valid_nearby = Vec::new();
  for ticket in nearby {
    if let Some(erring) = ticket.iter().find(|n| {
      !defs
        .values()
        .any(|[r0, r1]| r0.contains(n) || r1.contains(n))
    }) {
      error_rate += erring;
    } else {
      valid_nearby.push(ticket);
    }
  }

  println!("Part 1: {}", error_rate);

  let mut remaining = valid_nearby
    .iter()
    .map(|ticket| {
      ticket
        .into_iter()
        .map(|n| {
          defs
            .iter()
            .filter_map(|(k, [r0, r1])| {
              (r0.contains(n) || r1.contains(n)).then_some(k.as_str())
            })
            .collect::<HashSet<&str>>()
        })
        .collect_vec()
    })
    .reduce(|it0, it1| {
      it0
        .into_iter()
        .zip(it1.into_iter())
        .map(|(p0, p1)| p0.intersection(&p1).copied().collect())
        .collect()
    })
    .unwrap()
    .into_iter()
    .enumerate()
    .collect_vec();

  remaining.sort_by_key(|(_, s)| usize::MAX - s.len()); // sort by length descending
  let mut defs_idx = HashMap::new();
  while let Some((idx, defs)) = remaining.pop() {
    if defs.len() != 1 {
      println!("Ambiguity (idx {} could be {:?}), exiting now.", idx, defs);
      std::process::exit(1);
    }
    let name = defs.into_iter().exactly_one().unwrap();
    defs_idx.insert(name, idx);

    for set in remaining.iter_mut() {
      set.1.remove(name);
    }
  }

  for (def, idx) in defs_idx.iter() {
    println!("{def}: {}", you[*idx]);
  }

  let product = defs_idx
    .iter()
    .filter_map(|(k, v)| k.starts_with("departure").then_some(v))
    .map(|idx| you[*idx])
    .product::<usize>();

  println!("Part 2: {}", product);
}
