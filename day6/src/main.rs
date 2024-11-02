use std::{
  collections::{HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

type Point = (usize, usize);

fn manhattan(p1: Point, p2: Point) -> usize {
  p1.0.abs_diff(p2.0) + p1.1.abs_diff(p2.1)
}

fn main() {
  let file =
    File::open(std::env::args().nth(1).expect("Missing input file")).unwrap();
  let reader = BufReader::new(file);

  let regex = Regex::new(r"(\d+), (\d+)").unwrap();
  let points = reader
    .lines()
    .enumerate()
    .map(|(i, line)| {
      let line = line.unwrap();
      let caps = regex.captures(&line).unwrap();

      (
        i,
        (
          caps[1].parse::<usize>().unwrap(),
          caps[2].parse::<usize>().unwrap(),
        ),
      )
    })
    .collect::<HashMap<usize, Point>>();
  let max_x = *points.values().map(|(x, _)| x).max().unwrap() + 1;
  let max_y = *points.values().map(|(_, y)| y).max().unwrap() + 1;

  let mut associations: HashMap<Point, usize> = HashMap::new();
  let mut total_valid_points: Vec<Point> = Vec::new();
  for coord in (0..max_x)
    .map(|x| (0..max_y).map(move |y| (x, y)))
    .flatten()
  {
    if points
      .iter()
      .map(|(_, p)| manhattan(*p, coord))
      .sum::<usize>()
      < 10000
    {
      total_valid_points.push(coord);
    }
    let association = points
      .iter()
      .map(|(n, p)| (n, manhattan(*p, coord)))
      .min_set_by_key(|(_, d)| *d);
    if association.len() != 1 {
      continue;
    }
    associations.insert(coord, *association[0].0);
  }

  let excluded: HashSet<usize> = (0..max_x)
    .map(|x| [(x, 0), (x, max_y - 1)])
    .flatten()
    .chain((0..max_y).map(|y| [(0, y), (max_x - 1, y)]).flatten())
    .filter_map(|coord| associations.get(&coord).map(|v| *v))
    .collect();

  let biggest_area = associations
    .iter()
    .filter(|(_, k)| !excluded.contains(k))
    .fold(HashMap::new(), |mut map, (_, k)| {
      if !map.contains_key(k) {
        map.insert(k, 0);
      }
      *map.get_mut(k).unwrap() += 1;
      map
    })
    .into_values()
    .max()
    .unwrap();

  println!("Part 1: {:?}", biggest_area);
  println!("Part 2: {:?}", total_valid_points.len());
}
