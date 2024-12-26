use std::{
  collections::{HashMap, HashSet},
  io::{stdin, BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

fn main() {
  let reader = BufReader::new(stdin());

  let lines = reader.lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let regex = Regex::new(r"(\w+) to (\w+) = (\d+)").unwrap();
  let graph: HashMap<&str, HashMap<&str, usize>> =
    lines.iter().fold(HashMap::new(), |mut graph, line| {
      let caps = regex.captures(line).unwrap();
      let (src, dest, dist) = caps
        .iter()
        .skip(1)
        .map(|c| c.unwrap().as_str())
        .collect_tuple()
        .unwrap();
      let dist = dist.parse::<usize>().unwrap();
      graph.entry(src).or_default().insert(dest, dist);
      graph.entry(dest).or_default().insert(src, dist);
      graph
    });

  let (min_dist, max_dist) = graph
    .keys()
    .map(|loc| {
      let visited = [*loc].into_iter().collect();
      min_covering_dist(&graph, loc, visited, 0)
    })
    .reduce(|(min, max), (low, high)| (min.min(low), max.max(high)))
    .unwrap();

  println!("Part 1: {min_dist}");
  println!("Part 2: {max_dist}");
}

fn min_covering_dist<'g>(
  graph: &HashMap<&'g str, HashMap<&'g str, usize>>,
  src: &'g str,
  visited: HashSet<&'g str>,
  so_far: usize,
) -> (usize, usize) {
  graph
    .get(src)
    .unwrap()
    .iter()
    .filter(|(loc, _)| !visited.contains(*loc))
    .map(|(loc, dist)| {
      let mut next_visited = visited.clone();
      next_visited.insert(loc);
      min_covering_dist(graph, loc, next_visited, so_far + dist)
    })
    .reduce(|(min, max), (low, high)| (min.min(low), max.max(high)))
    .unwrap_or((so_far, so_far))
}
