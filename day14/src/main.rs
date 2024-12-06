use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

struct Reactant<'a> {
  name: &'a str,
  amount: usize,
}

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let regex = Regex::new(r"(\d+) ([A-Z]+)").unwrap();
  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();

  let reactions: HashMap<&str, (usize, Vec<Reactant>)> = lines
    .iter()
    .map(|line| {
      let mut components = regex
        .captures_iter(line)
        .map(|caps| {
          let amount = caps.get(1).unwrap().as_str().parse::<usize>().unwrap();
          let name = caps.get(2).unwrap().as_str();
          Reactant { name, amount }
        })
        .collect_vec();

      let product = components.pop().unwrap();

      (product.name, (product.amount, components))
    })
    .collect();

  let mut excess: HashMap<&str, usize> = HashMap::new();
  let ore = calc_ore_required("FUEL", 1, &reactions, &mut excess);
  println!("Part 1: {}", ore);

  const TOTAL_ORE: usize = 1000000000000;

  let (mut under, mut over) = (TOTAL_ORE / ore, TOTAL_ORE);
  let fuel = loop {
    if over - under <= 1 {
      break under;
    }
    excess.clear();
    let fuel = (over + under) / 2;
    let ore = calc_ore_required("FUEL", fuel, &reactions, &mut excess);
    if ore > TOTAL_ORE {
      over = fuel;
    } else {
      under = fuel;
    }
  };

  println!("Part 2: {}", fuel);
}

fn calc_ore_required<'a>(
  target: &'a str,
  mut amount_desired: usize,
  reactions: &HashMap<&'a str, (usize, Vec<Reactant<'a>>)>,
  //cache: &mut HashMap<&'a str, (usize, usize)>,
  excess: &mut HashMap<&'a str, usize>,
  //total_made: &mut HashMap<&'a str, usize>,
) -> usize {
  if let Some(amount_excess) = excess.get_mut(target) {
    let offset = (*amount_excess).min(amount_desired);
    if offset > 0 {
      //println!(">> {target} took {offset} from excess");
      *amount_excess -= offset;
      amount_desired -= offset;
      if amount_desired == 0 {
        //println!("{target} => from excess");
        return 0;
      }
    }
  }

  if target == "ORE" {
    //println!("{target} #> {amount_desired}");
    return amount_desired;
  }

  let (per, reactants) = reactions.get(target).unwrap();
  let num_reactions = calc_num_reactions(amount_desired, *per);
  let ore = reactants
    .iter()
    .map(|Reactant { name, amount }| {
      calc_ore_required(name, *amount * num_reactions, reactions, excess)
    })
    .sum::<usize>();

  let amount_made = per * num_reactions;
  //println!(">> {amount_made} made of {target}");
  //*total_made.entry(target).or_default() += amount_made;
  let amount_excess = amount_made - amount_desired;
  if amount_excess != 0 {
    //println!(">> {target} left {amount_excess} for excess");
  }
  *excess.entry(target).or_default() += amount_excess;
  //println!("{target} => {num_reactions} * {ore}");
  ore
}

fn calc_num_reactions(desired: usize, per: usize) -> usize {
  ((desired / per) - (desired % per == 0) as usize) + 1
}
