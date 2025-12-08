use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;

struct JunctionBox {
  x: i64,
  y: i64,
  z: i64,
}

impl JunctionBox {
  fn dist_index(&self, other: &Self) -> i64 {
    (self.x - other.x).pow(2)
      + (self.y - other.y).pow(2)
      + (self.z - other.z).pow(2)
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let boxes = lines
    .iter()
    .map(|line| {
      let (x, y, z) = line
        .split(',')
        .map(|n| n.parse::<i64>().unwrap())
        .collect_tuple()
        .unwrap();

      JunctionBox { x, y, z }
    })
    .collect_vec();

  let mut pair_distances = boxes
    .iter()
    .enumerate()
    .tuple_combinations()
    .map(|((i, l), (j, r))| (l.dist_index(r), (i, j)))
    .collect_vec();

  pair_distances.sort_unstable_by_key(|(dist, _)| *dist);

  let mut owned: HashMap<usize, usize> = HashMap::new();
  let mut subgraphs: Vec<HashSet<usize>> = Vec::new();

  for (pair_i, (_, (l, r))) in pair_distances.iter().enumerate() {
    let l_group = owned.get(l).copied();
    let r_group = owned.get(r).copied();

    let updated = match (l_group, r_group) {
      (Some(l_group), Some(r_group)) => {
        let moving_group = std::mem::take(&mut subgraphs[r_group]);
        for member in moving_group.iter() {
          *owned.get_mut(member).unwrap() = l_group;
        }
        subgraphs[l_group].extend(moving_group);

        subgraphs[l_group].insert(*r);
        owned.insert(*r, l_group);

        l_group
      }
      (Some(l_group), None) => {
        subgraphs[l_group].insert(*r);
        owned.insert(*r, l_group);

        l_group
      }
      (None, Some(r_group)) => {
        subgraphs[r_group].insert(*l);
        owned.insert(*l, r_group);

        r_group
      }
      (None, None) => {
        let next_group = subgraphs.len();
        owned.insert(*r, next_group);
        owned.insert(*l, next_group);

        subgraphs.push([*l, *r].into_iter().collect());

        next_group
      }
    };

    if pair_i == 1000 {
      let solution = subgraphs
        .iter()
        .map(HashSet::len)
        .sorted()
        .rev()
        .take(3)
        .product::<usize>();

      println!("Part 1: {}", solution);
    }

    if subgraphs[updated].len() == boxes.len() {
      println!("Part 2: {}", boxes[*l].x * boxes[*r].x);
      break;
    }
  }
}
