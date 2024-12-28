use std::{collections::HashMap, io::stdin};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let nums = Regex::new(r"\d+").unwrap();
  let targets = Regex::new(r"(?:bot|output) \d+").unwrap();
  let mut bots: HashMap<&str, Vec<usize>> = HashMap::new();
  let mut bot_rules: HashMap<&str, (&str, &str)> = HashMap::new();

  for line in lines.iter() {
    if line.starts_with("value") {
      let val = nums.find(&line).unwrap().as_str().parse().unwrap();
      let bot = targets.find(&line).unwrap().as_str();
      bots.entry(bot).or_default().push(val);
      continue;
    }
    let (src, low_target, high_target) = targets
      .find_iter(line)
      .map(|m| m.as_str())
      .collect_tuple()
      .unwrap();
    bot_rules.insert(src, (low_target, high_target));
  }

  let mut cmp_bot = "";
  loop {
    let (robot, microchips) = match bots
      .iter_mut()
      .find(|(_, microchips)| microchips.len() == 2)
    {
      Some(v) => v,
      None => break,
    };
    let (low, high) = std::mem::take(microchips)
      .into_iter()
      .minmax()
      .into_option()
      .unwrap();
    if low == 17 && high == 61 {
      cmp_bot = robot;
    }

    let (low_target, high_target) = bot_rules.get(robot).unwrap();
    bots.entry(&low_target).or_default().push(low);
    bots.entry(&high_target).or_default().push(high);
  }

  println!("Part 1: {cmp_bot}");
  println!(
    "Part 2: {}",
    (0..3)
      .map(|n| bots.get(format!("output {}", n).as_str()).unwrap()[0])
      .product::<usize>()
  )
}
