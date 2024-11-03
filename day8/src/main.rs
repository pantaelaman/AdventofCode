use std::{fs::File, io::Read};

#[derive(Debug)]
struct Tree {
  children: Vec<Tree>,
  metadata: Vec<usize>,
}

impl Tree {
  pub fn sum_metadata(&self) -> usize {
    self.metadata.iter().sum::<usize>()
      + self
        .children
        .iter()
        .map(|c| c.sum_metadata())
        .sum::<usize>()
  }

  pub fn get_value(&self) -> usize {
    if self.children.is_empty() {
      return self.metadata.iter().sum::<usize>();
    }
    self
      .metadata
      .iter()
      .filter_map(|i| self.children.get(*i - 1).map(|n| n.get_value()))
      .sum::<usize>()
  }
}

fn parse_tree<I>(data: &mut I) -> Tree
where
  I: Iterator<Item = usize>,
{
  let children_len = data.next().unwrap();
  let metadata_len = data.next().unwrap();
  let mut children = Vec::with_capacity(children_len);
  let mut metadata = Vec::with_capacity(metadata_len);

  for _ in 0..children_len {
    children.push(parse_tree(data));
  }

  for _ in 0..metadata_len {
    metadata.push(data.next().unwrap());
  }

  Tree { children, metadata }
}

fn main() {
  let mut file =
    File::open(std::env::args().nth(1).expect("Missing input file")).unwrap();
  let mut content = String::new();
  file.read_to_string(&mut content).unwrap();
  let mut data = content
    .split_whitespace()
    .map(|s| s.parse::<usize>().unwrap());

  let tree = parse_tree(&mut data);
  println!("Part 1: {}", tree.sum_metadata());
  println!("Part 2: {}", tree.get_value());
}
