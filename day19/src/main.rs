#![feature(ascii_char)]
#![feature(slice_split_once)]
use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum Element<'k> {
  Real(&'k str),
  SpecTERM(&'k str),
  SpecBIN(usize),
}

impl<'k> Element<'k> {
  fn as_str(&self) -> &'k str {
    match self {
      Self::Real(s) | Self::SpecTERM(s) => s,
      Self::SpecBIN(_) => unimplemented!(),
    }
  }
}

#[derive(Clone, Copy, Debug)]
enum RuleTarget<'k> {
  Binary(Element<'k>, Element<'k>),
  Terminal(&'k str),
}

type Rule<'k> = (Element<'k>, RuleTarget<'k>);

fn split_elements<'k>(inp: &'k str) -> impl Iterator<Item = Element<'k>> {
  struct ElementSplit<'k> {
    src: &'k str,
  }

  impl<'k> Iterator for ElementSplit<'k> {
    type Item = &'k str;

    fn next(&mut self) -> Option<Self::Item> {
      if self.src.is_empty() {
        return None;
      }
      match self.src[1..].find(|c: char| c.is_uppercase()) {
        Some(idx) => {
          let (front, back) = self.src.split_at(idx + 1);
          self.src = back;
          Some(front)
        }
        None => {
          let el = self.src;
          self.src = "";
          Some(el)
        }
      }
    }
  }

  (ElementSplit { src: inp }).map(|v| Element::Real(v))
}

fn reduce_term<'k>(
  rules: Vec<(Element<'k>, Vec<Element<'k>>)>,
) -> Vec<(Element<'k>, Vec<Element<'k>>)> {
  let nonterminals: HashSet<Element<'k>> =
    rules.iter().map(|(l, _)| l).copied().collect();

  let mut new_rules = HashSet::new();
  for (src, dest) in rules {
    let mut new_dest: Vec<Element<'k>> = Vec::new();
    for el in dest {
      if !nonterminals.contains(&el) {
        new_rules.insert((Element::SpecTERM(&el.as_str()), vec![el]));
        new_dest.push(Element::SpecTERM(&el.as_str()));
      } else {
        new_dest.push(el);
      }
    }
    new_rules.insert((src, new_dest));
  }
  new_rules.into_iter().collect()
}

fn reduce_bin<'k>(
  rules: Vec<(Element<'k>, Vec<Element<'k>>)>,
) -> Vec<Rule<'k>> {
  let mut new_rules = Vec::new();

  fn reduce_dests<'k>(
    src: Element<'k>,
    dest: &[Element<'k>],
    new_rules: &mut Vec<Rule<'k>>,
  ) {
    static mut BIN_ID: usize = 0;
    match dest.len() {
      2 => new_rules.push((src, RuleTarget::Binary(dest[0], dest[1]))),
      1 => new_rules.push((src, RuleTarget::Terminal(&dest[0].as_str()))),
      _ => {
        let overflow = Element::SpecBIN(unsafe {
          BIN_ID += 1;
          BIN_ID
        });
        new_rules.push((src, RuleTarget::Binary(dest[0], overflow)));
        reduce_dests(overflow, &dest[1..], new_rules);
      }
    }
  }

  for (src, dest) in rules {
    reduce_dests(src, &dest, &mut new_rules);
  }
  new_rules
}

fn cyk<'k>(
  inp: Vec<Element<'k>>,
  rules: Vec<Rule<'k>>,
  start: Element<'k>,
) -> HashMap<(usize, usize, Element<'k>), Vec<(usize, Element<'k>, Element<'k>)>>
{
  let n = inp.len();
  let mut P: HashSet<(usize, usize, Element<'k>)> = HashSet::new();
  let mut back: HashMap<
    (usize, usize, Element<'k>),
    Vec<(usize, Element<'k>, Element<'k>)>,
  > = HashMap::new();

  for s in 0..n {
    for (v, production) in rules.iter() {
      match production {
        RuleTarget::Terminal(t) => {
          if *t == inp[s].as_str() {
            P.insert((0, s, *v));
          }
        }
        _ => {}
      }
    }
  }

  for l in 1..n {
    for s in 0..(n - l + 1) {
      for p in 0..(l - 1) {
        for (a, production) in rules.iter() {
          match production {
            RuleTarget::Binary(b, c) => {
              if P.contains(&(p, s, *b)) && P.contains(&(l - p, s + p, *c)) {
                P.insert((l, s, *a));
                back.entry((l, s, *a)).or_default().push((p, *b, *c));
              }
            }
            _ => continue,
          }
        }
      }
    }
  }

  println!("{P:?}");
  println!("{back:?}");

  if P.contains(&(n - 1, 0, start)) {
    return back;
  } else {
    panic!("Grammar failed");
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let (raw_replacements, start) = lines.split_once(|e| e.is_empty()).unwrap();
  let replacements: HashMap<&str, HashSet<&str>> = raw_replacements
    .iter()
    .map(|l| l.split_once(" => ").unwrap())
    .fold(HashMap::new(), |mut acc, (src, dest)| {
      acc.entry(src).or_default().insert(dest);
      acc
    });

  println!("Part 1: {}", replace(&start[0], &replacements).len());
  let rules = raw_replacements
    .iter()
    .map(|l| l.split_once(" => ").unwrap())
    .map(|(src, dest)| (Element::Real(src), split_elements(dest).collect_vec()))
    .collect_vec();
  let rules = reduce_term(rules);
  let rules = reduce_bin(rules);
  println!("{rules:?}");
  let back = cyk(
    split_elements(&start[0]).collect_vec(),
    rules,
    Element::Real("e"),
  );
  println!("{back:?}");
}

fn replace<'k>(
  start: &String,
  replacements: &HashMap<&'k str, HashSet<&'k str>>,
) -> HashSet<String> {
  let mut possibilites = HashSet::new();
  for (src, dest) in replacements
    .iter()
    .flat_map(|(k, vs)| vs.iter().map(move |v| (k, v)))
  {
    for (idx, _) in start.match_indices(src) {
      let (front, back) = start.split_at(idx);
      possibilites
        .insert(front.to_owned() + dest + back.strip_prefix(src).unwrap());
    }
  }
  possibilites
}

fn fabricate_molecule<'k>(
  start: String,
  target: String,
  replacements: &HashMap<&'k str, HashSet<&'k str>>,
) -> usize {
  let mut steps = 0;
  let mut frontier = vec![start];
  let mut visited = HashSet::new();
  while !frontier.is_empty() {
    for start in std::mem::take(&mut frontier).into_iter() {
      if start == target {
        return steps;
      }
      if visited.contains(&start) {
        continue;
      }
      frontier.extend(replace(&start, replacements).into_iter());
      visited.insert(start);
    }
    steps += 1;
  }
  0
}

fn invert_replacements<'k>(
  replacements: &HashMap<&'k str, HashSet<&'k str>>,
) -> HashMap<&'k str, HashSet<&'k str>> {
  replacements
    .iter()
    .flat_map(|(k, vs)| vs.iter().map(move |v| (k, v)))
    .fold(HashMap::new(), |mut acc, (dest, src)| {
      acc.entry(src).or_default().insert(dest);
      acc
    })
}
