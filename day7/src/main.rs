use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;
use regex::Regex;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct BagType {
  descriptor: String,
  colour: String,
}

#[derive(Debug)]
struct BagContents {
  amount: usize,
  bag_type: BagType,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();

  let bag = Regex::new(r"(\w+) (\w+) bag").unwrap();
  let num = Regex::new(r"\d+").unwrap();
  let bag_rules: HashMap<BagType, Vec<BagContents>> = lines
    .iter()
    .map(|line| {
      let mut bags = bag.captures_iter(&line).map(|caps| BagType {
        descriptor: caps[1].to_owned(),
        colour: caps[2].to_owned(),
      });
      let amounts = num
        .find_iter(&line)
        .map(|n| n.as_str().parse::<usize>().unwrap());

      let key = bags.next().unwrap();

      let contents = amounts
        .zip(bags)
        .map(|(amount, bag)| BagContents {
          amount,
          bag_type: bag,
        })
        .collect_vec();

      (key, contents)
    })
    .collect();

  let inverted_rules: HashMap<BagType, Vec<BagContents>> = bag_rules
    .iter()
    .fold(HashMap::new(), |mut acc, (parent, children)| {
      for BagContents { amount, bag_type } in children {
        acc.entry(bag_type.clone()).or_default().push(BagContents {
          amount: *amount,
          bag_type: parent.clone(),
        });
      }
      acc
    });

  let starting_bag_type = BagType {
    descriptor: String::from("shiny"),
    colour: String::from("gold"),
  };

  let mut head = vec![starting_bag_type.clone()];
  let mut visited = HashSet::new();

  while !head.is_empty() {
    for node in std::mem::take(&mut head) {
      if let Some(parents) = inverted_rules.get(&node) {
        head.extend(
          parents
            .iter()
            .filter_map(|BagContents { bag_type, .. }| {
              (!visited.contains(bag_type)).then_some(bag_type)
            })
            .cloned(),
        )
      };
      visited.insert(node);
    }
  }

  println!("Part 1: {}", visited.len() - 1); // -1 for the shiny gold bag itself

  let mut cache: HashMap<BagType, usize> = HashMap::new();

  fn dfs(
    node: BagType,
    rules: &HashMap<BagType, Vec<BagContents>>,
    cache: &mut HashMap<BagType, usize>,
  ) -> usize {
    if let Some(amount) = cache.get(&node) {
      return *amount;
    }

    let amount = rules
      .get(&node)
      .unwrap()
      .iter()
      .map(|node| node.amount * (1 + dfs(node.bag_type.clone(), rules, cache)))
      .sum();

    cache.insert(node, amount);

    amount
  }

  println!("Part 2: {}", dfs(starting_bag_type, &bag_rules, &mut cache));
}
