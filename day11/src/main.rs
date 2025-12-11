use std::{collections::HashMap, io::stdin};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let graph = lines
    .iter()
    .map(|line| {
      let (key, outputs) = line.split(": ").collect_tuple().unwrap();
      let outputs = outputs.split_whitespace().collect_vec();
      (key, outputs)
    })
    .collect::<HashMap<_, _>>();

  let mut frontier = vec![vec!["you"]];
  let mut complete = 0;

  while !frontier.is_empty() {
    for path in std::mem::take(&mut frontier) {
      for edge in graph[path.last().unwrap()].iter() {
        if *edge == "out" {
          complete += 1;
          continue;
        }

        let mut new_path = path.clone();
        new_path.push(edge);
        frontier.push(new_path);
      }
    }
  }

  println!("Part 1: {complete}");

  let mut memo = HashMap::new();
  let routes = count_routes(
    Path {
      last: "svr",
      fft: false,
      dac: false,
    },
    "out",
    &graph,
    &mut memo,
  );
  println!("Part 2: {routes}");
}

#[derive(Hash, PartialEq, Eq, Debug)]
struct Path<'a> {
  last: &'a str,
  fft: bool,
  dac: bool,
}

fn count_routes<'a, 'b>(
  path: Path<'a>,
  dest: &'a str,
  graph: &'a HashMap<&'a str, Vec<&'a str>>,
  memo: &'b mut HashMap<Path<'a>, usize>,
) -> usize {
  if let Some(v) = memo.get(&path) {
    return *v;
  }

  let Some(edges) = graph.get(path.last) else {
    return 0;
  };

  let res = edges
    .iter()
    .map(|edge| {
      if *edge == dest {
        if path.dac && path.fft {
          return 1;
        } else {
          return 0;
        }
      }

      let new_path = Path {
        last: edge,
        fft: path.fft || *edge == "fft",
        dac: path.dac || *edge == "dac",
      };

      count_routes(new_path, dest, graph, memo)
    })
    .sum();

  memo.insert(path, res);

  res
}
