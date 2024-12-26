use std::{
  collections::{BTreeMap, BTreeSet, HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader, Write},
  iter::Successors,
};

use bimap::BiHashMap;
use itertools::Itertools;
use pariter::{scope, IteratorExt};
use pathfinding::prelude::dijkstra;

// don't have to worry about overlap cause of the border
type Position = (usize, usize);

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let reader = BufReader::new(file);

  let mut walls = HashSet::new();
  let mut keys = BiHashMap::new();
  let mut doors = BiHashMap::new();
  let mut entrance = (0, 0);
  for (y, line) in reader.lines().map(|line| line.unwrap()).enumerate() {
    for (x, c) in line.chars().enumerate() {
      match c {
        '.' => continue,
        '#' => {
          walls.insert((x, y));
        }
        '@' => {
          entrance = (x, y);
        }
        c => {
          if c.is_ascii_lowercase() {
            keys.insert((x, y), c);
          } else {
            doors.insert((x, y), c.to_ascii_lowercase());
          }
        }
      }
    }
  }

  //println!("Walls: {walls:?}");
  //println!("Keys: {keys:?}");
  //println!("Doors: {doors:?}");
  //println!("Entrance: {entrance:?}");

  //  let mut frontier = vec![entrance];
  //  let mut visited = HashSet::new();
  //  let mut steps = 0;
  //  'bfs: while !keys.is_empty() {
  //    for (x, y) in std::mem::take(&mut frontier) {
  //      if let Some(key) = keys.remove(&(x, y)) {
  //        visited.clear();
  //        frontier.clear();
  //        frontier.push((x, y));
  //        doors.remove_by_right(&key.to_ascii_uppercase());
  //        continue 'bfs;
  //      }
  //      visited.insert((x, y));
  //      let successors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  //        .into_iter()
  //        // .inspect(|p| println!("{p:?}"))
  //        .filter(|p| {
  //          !visited.contains(p) && !walls.contains(p) && !doors.contains_left(p)
  //        });
  //      // .inspect(|p| println!("{p:?} made it"));
  //      frontier.extend(successors);
  //    }
  //    steps += 1;
  //  }
  keys.insert(entrance, '@');
  let keys_to_doors = map_passed_doors(entrance, &walls, &keys, &doors);
  println!("{:?}", keys_to_doors);
  let key_dists = calc_key_distances(&walls, &keys);
  println!("{:?}", key_dists);
  //let steps = reachable_keys(entrance, &walls, &keys, &doors, HashSet::new());
  let steps =
    collapse_dists_and_doors(entrance, &keys, &keys_to_doors, &key_dists);

  println!("Part 1: {}", steps);
}

fn collapse_dists_and_doors(
  start: Position,
  keys: &BiHashMap<Position, char>,
  keys_to_doors: &HashMap<char, HashSet<char>>,
  key_dists: &HashMap<char, HashMap<char, usize>>,
) -> usize {
  #[derive(Hash, Debug, PartialEq, Eq, Clone)]
  struct Node {
    position: Position,
    collected: BTreeSet<char>,
  }

  let mut frontier: BTreeMap<usize, Vec<Node>> = BTreeMap::new();
  let mut init_set = BTreeSet::new();
  init_set.insert('@');
  frontier.insert(
    0,
    vec![Node {
      position: start,
      collected: init_set,
    }],
  );

  let mut seen: HashSet<Node> = HashSet::new();

  loop {
    //println!("{frontier:?}");
    let (prior_dist, nodes) = frontier.first_entry().unwrap().remove_entry();
    for node in nodes {
      if !seen.insert(node.clone()) {
        //println!("Skipped!");
        continue;
      }

      if node.collected.len() == keys.len() {
        return prior_dist;
      }
      //println!("{:?} ({:?})", node.position, node.collected);
      let key = keys.get_by_left(&node.position).unwrap();
      //println!("{key:?}");
      let local_dists = key_dists.get(key).unwrap();
      //println!("{local_dists:?}");
      for (new_key, dist) in local_dists.iter().filter(|(new_key, _)| {
        !node.collected.contains(new_key)
          && keys_to_doors
            .get(new_key)
            .unwrap()
            .iter()
            .all(|d| node.collected.contains(d))
      }) {
        //println!("Next: {new_key:?}, ({dist})");
        let mut new_collected = node.collected.clone();
        new_collected.insert(*new_key);
        frontier.entry(dist + prior_dist).or_default().push(Node {
          position: *keys.get_by_right(new_key).unwrap(),
          collected: new_collected,
        });
      }
      //println!();
    }
  }
}

fn map_passed_doors(
  start: Position,
  walls: &HashSet<Position>,
  keys: &BiHashMap<Position, char>,
  doors: &BiHashMap<Position, char>,
) -> HashMap<char, HashSet<char>> {
  let mut frontier = vec![(start, HashSet::<char>::new())];
  let mut visited = HashSet::new();
  let mut keys_to_doors = HashMap::new();
  while !frontier.is_empty() {
    for ((x, y), mut passed_doors) in std::mem::take(&mut frontier) {
      visited.insert((x, y));
      if let Some(key) = keys.get_by_left(&(x, y)) {
        keys_to_doors.insert(*key, passed_doors.clone());
        passed_doors.insert(*key); // keys are doors too!
      } else if let Some(door) = doors.get_by_left(&(x, y)) {
        passed_doors.insert(*door);
      }
      let successors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        .into_iter()
        .filter(|p| !visited.contains(p) && !walls.contains(p))
        .map(|s| (s, passed_doors.clone()));
      frontier.extend(successors);
    }
  }
  keys_to_doors
}

fn calc_key_distances(
  walls: &HashSet<Position>,
  keys: &BiHashMap<Position, char>,
) -> HashMap<char, HashMap<char, usize>> {
  let mut key_distances: HashMap<char, HashMap<char, usize>> = HashMap::new();
  //println!("\n--- Dists ---");
  for ((p_a, key_a), (p_b, key_b)) in keys.iter().tuple_combinations() {
    //println!("{key_a} <-> {key_b}");
    let dist = path_length(*p_a, *p_b, walls).unwrap();
    //println!("{dist}");
    key_distances
      .entry(*key_a)
      .or_default()
      .insert(*key_b, dist);
    key_distances
      .entry(*key_b)
      .or_default()
      .insert(*key_a, dist);
    //println!();
  }
  //println!("\n--- End ---");
  key_distances
}

fn path_length(
  start: Position,
  target: Position,
  walls: &HashSet<Position>,
) -> Option<usize> {
  let mut frontier = vec![start];
  let mut visited = HashSet::new();
  let mut steps = 0;
  while !frontier.is_empty() {
    for (x, y) in std::mem::take(&mut frontier) {
      if (x, y) == target {
        return Some(steps);
      }
      visited.insert((x, y));
      let successors = raw_successors((x, y))
        .filter(|p| !visited.contains(p) && !walls.contains(p));
      frontier.extend(successors);
    }
    steps += 1;
  }
  None
}

fn raw_successors((x, y): Position) -> impl Iterator<Item = Position> {
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)].into_iter()
}
