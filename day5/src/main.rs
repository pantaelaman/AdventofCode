use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  let (rules, printings) =
    lines.split(|el| el.is_empty()).collect_tuple().unwrap();

  let ordering: HashMap<usize, HashSet<usize>> = rules
    .into_iter()
    .map(|rule| {
      rule
        .split('|')
        .map(|s| s.parse::<usize>().unwrap())
        .collect_tuple()
        .unwrap()
    })
    .fold(HashMap::new(), |mut ordering, (prior, latter)| {
      ordering.entry(latter).or_default().insert(prior);
      ordering
    });

  let mut valids = HashSet::new();

  let printings = printings
    .into_iter()
    .map(|printing| {
      printing
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect_vec()
    })
    .collect_vec();

  let valid_printings: usize = printings
    .iter()
    .enumerate()
    .filter_map(|(idx, pages)| {
      let mut printed = Vec::new();
      pages
        .iter()
        .all(|page| {
          let test = ordering
            .get(&page)
            .map(|priors| {
              priors
                .iter()
                .filter(|p| pages.contains(p))
                .all(|p| printed.contains(p))
            })
            .unwrap_or(true);
          printed.push(*page);
          test
        })
        .then(|| (idx, printed[printed.len() / 2]))
    })
    .map(|(idx, v)| {
      valids.insert(idx);
      v
    })
    .sum();

  println!("Part 1: {}", valid_printings);

  let fixed_total: usize = printings
    .iter()
    .enumerate()
    .filter_map(|(idx, printing)| (!valids.contains(&idx)).then_some(printing))
    .map(|pages| {
      let mut deptree: HashMap<usize, HashSet<usize>> = ordering
        .iter()
        .filter(|(latter, _)| pages.contains(latter))
        .map(|(latter, priors)| {
          (
            *latter,
            priors
              .iter()
              .filter(|prior| pages.contains(prior))
              .copied()
              .collect(),
          )
        })
        .collect();

      let mut printed = Vec::new();

      while !deptree.is_empty() {
        let bottom = pages
          .iter()
          .filter(|page| !printed.contains(page))
          .filter(|page| {
            deptree
              .get(&page)
              .map(|priors| priors.iter().all(|prior| printed.contains(&prior)))
              .unwrap_or(true)
          })
          .exactly_one()
          .unwrap();

        printed.push(bottom);
        deptree.remove(bottom);
      }

      printed[printed.len() / 2]
    })
    .sum();

  println!("Part 2: {}", fixed_total);
}
