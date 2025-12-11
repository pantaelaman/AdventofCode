use std::{
  cmp,
  collections::{BinaryHeap, HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum Node {
  Start((i32, i32)),
  Key(char),
  Door(char),
}

enum GridPoint {
  Node(Node),
  Wall,
}

struct Positioned<T> {
  pos: (usize, usize),
  data: T,
}

#[derive(PartialEq, Eq)]
struct Head {
  keys: Vec<char>,
  node: Node,
  steps: usize,
}

impl PartialOrd for Head {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.steps.partial_cmp(&other.steps)
  }
}

impl Ord for Head {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.steps.cmp(&other.steps)
  }
}

#[derive(PartialEq, Eq)]
struct Heads {
  keys: Vec<char>,
  nodes: [Node; 4],
  steps: usize,
}

impl PartialOrd for Heads {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.steps.partial_cmp(&other.steps)
  }
}

impl Ord for Heads {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.steps.cmp(&other.steps)
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let grid = lines
    .iter()
    .map(|line| line.chars().collect_vec())
    .collect_vec();

  let graph = build_graph(&grid);

  let n_keys = graph.keys().filter(|n| matches!(n, Node::Key(_))).count();

  let mut seen: HashSet<(Vec<char>, Node)> = HashSet::new();
  let mut queue: BinaryHeap<cmp::Reverse<Head>> = BinaryHeap::new();
  queue.push(cmp::Reverse(Head {
    keys: Vec::new(),
    node: *graph.keys().find(|k| matches!(k, Node::Start(_))).unwrap(),
    steps: 0,
  }));

  let steps = loop {
    let Some(cmp::Reverse(head)) = queue.pop() else {
      panic!("no path found");
    };
    if !seen.insert((head.keys.clone(), head.node)) {
      continue;
    }

    if head.keys.len() == n_keys {
      break head.steps;
    }

    for (neighbour, addl) in graph[&head.node].iter() {
      match *neighbour {
        Node::Start(_) => {
          queue.push(cmp::Reverse(Head {
            keys: head.keys.clone(),
            node: *neighbour,
            steps: head.steps + addl,
          }));
        }
        Node::Key(c) => queue.push(cmp::Reverse(Head {
          keys: if head.keys.contains(&c) {
            head.keys.clone()
          } else {
            let mut keys = head.keys.clone();
            keys.push(c);
            keys.sort_unstable();
            keys
          },
          node: *neighbour,
          steps: head.steps + addl,
        })),
        Node::Door(c) => {
          if head.keys.contains(&c) {
            queue.push(cmp::Reverse(Head {
              keys: head.keys.clone(),
              node: *neighbour,
              steps: head.steps + addl,
            }))
          } else {
            continue;
          }
        }
      }
    }
  };

  println!("Part 1: {}", steps);

  let grid = {
    let mut grid = grid;
    let (sx, sy) = grid
      .iter()
      .enumerate()
      .find_map(|(y, row)| {
        row
          .iter()
          .enumerate()
          .find_map(|(x, c)| (*c == '@').then_some((x, y)))
      })
      .unwrap();

    grid[sy][sx] = '#';
    grid[sy - 1][sx] = '#';
    grid[sy + 1][sx] = '#';
    grid[sy][sx - 1] = '#';
    grid[sy][sx + 1] = '#';
    grid[sy + 1][sx + 1] = '@';
    grid[sy - 1][sx + 1] = '@';
    grid[sy + 1][sx - 1] = '@';
    grid[sy - 1][sx - 1] = '@';

    grid
  };

  let graph = build_graph(&grid);

  let mut seen: HashSet<(Vec<char>, [Node; 4])> = HashSet::new();
  let mut queue: BinaryHeap<cmp::Reverse<Heads>> = BinaryHeap::new();
  let starts = graph
    .keys()
    .filter(|k| matches!(k, Node::Start(_)))
    .copied()
    .collect_vec();

  queue.push(cmp::Reverse(Heads {
    keys: Vec::new(),
    nodes: starts[..4].try_into().unwrap(),
    steps: 0,
  }));

  let steps = loop {
    let Some(cmp::Reverse(heads)) = queue.pop() else {
      panic!("no path found");
    };
    if !seen.insert((heads.keys.clone(), heads.nodes)) {
      continue;
    }

    if heads.keys.len() == n_keys {
      break heads.steps;
    }

    for (i, node) in heads.nodes.iter().copied().enumerate() {
      for (neighbour, addl) in graph[&node].iter() {
        let next_nodes = {
          let mut next = heads.nodes.clone();
          next[i] = *neighbour;
          next
        };

        match *neighbour {
          Node::Start(_) => {
            queue.push(cmp::Reverse(Heads {
              keys: heads.keys.clone(),
              nodes: next_nodes,
              steps: heads.steps + addl,
            }));
          }
          Node::Key(c) => queue.push(cmp::Reverse(Heads {
            keys: if heads.keys.contains(&c) {
              heads.keys.clone()
            } else {
              let mut keys = heads.keys.clone();
              keys.push(c);
              keys.sort_unstable();
              keys
            },
            nodes: next_nodes,
            steps: heads.steps + addl,
          })),
          Node::Door(c) => {
            if heads.keys.contains(&c) {
              queue.push(cmp::Reverse(Heads {
                keys: heads.keys.clone(),
                nodes: next_nodes,
                steps: heads.steps + addl,
              }))
            } else {
              continue;
            }
          }
        }
      }
    }
  };

  println!("Part 2: {steps}");
}

fn build_graph(grid: &Vec<Vec<char>>) -> HashMap<Node, Vec<(Node, usize)>> {
  let nodes: HashMap<(i32, i32), GridPoint> = grid
    .iter()
    .enumerate()
    .flat_map(|(y, row)| {
      row.iter().enumerate().filter_map(move |(x, c)| {
        let xy = (x as i32, y as i32);
        if *c == '@' {
          Some((xy, GridPoint::Node(Node::Start(xy))))
        } else if *c == '#' {
          Some((xy, GridPoint::Wall))
        } else if c.is_ascii_uppercase() {
          Some((xy, GridPoint::Node(Node::Door(c.to_ascii_lowercase()))))
        } else if c.is_ascii_lowercase() {
          Some((xy, GridPoint::Node(Node::Key(*c))))
        } else {
          None
        }
      })
    })
    .collect();

  let mut graph: HashMap<Node, Vec<(Node, usize)>> = HashMap::new();

  for (xy, node) in nodes.iter().filter_map(|(xy, point)| match point {
    GridPoint::Node(node) => Some((xy, node)),
    _ => None,
  }) {
    let mut frontier = vec![(*xy, 0)];
    let mut seen = HashSet::new();
    while !frontier.is_empty() {
      for ((x, y), steps) in std::mem::take(&mut frontier) {
        let steps = steps + 1;
        seen.insert((x, y));

        let neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)];
        for neighbour in neighbours {
          if seen.contains(&neighbour) {
            continue;
          }

          match nodes.get(&neighbour) {
            Some(GridPoint::Wall) => {}
            Some(GridPoint::Node(next)) => {
              graph.entry(*node).or_default().push((*next, steps));
            }
            None => {
              frontier.push((neighbour, steps));
            }
          }
        }
      }
    }
  }

  graph
}
