use std::{
  collections::HashMap,
  hash::Hash,
  io::{stdin, Write},
};

use either::Either::{self, Left, Right};
use itertools::Itertools;
use ordermap::OrderSet;
use regex::Regex;
use uuid::Uuid;

#[derive(Debug, Clone)]
struct Rule {
  contents: Vec<Either<Uuid, char>>,
}

#[derive(Debug, Clone, Copy)]
struct PState<'r> {
  rule_uuid: &'r Uuid,
  rule_idx: usize,
  rule: &'r Rule,
  pos: usize,
  origin: usize,
}

impl<'r> Hash for PState<'r> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.rule_uuid.hash(state);
    self.rule_idx.hash(state);
    self.pos.hash(state);
    self.origin.hash(state);
  }
}

impl<'r> PartialEq for PState<'r> {
  fn eq(&self, other: &Self) -> bool {
    self.rule_uuid == other.rule_uuid
      && self.rule_idx == other.rule_idx
      && self.pos == other.pos
      && self.origin == other.origin
  }
}

impl<'r> Eq for PState<'r> {}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let mut mapping: HashMap<usize, Uuid> = HashMap::new();
  let mut rules: HashMap<Uuid, Vec<Rule>> = HashMap::new();

  let schar_match = Regex::new(r#""(\w)""#).unwrap();

  for rule_line in lines.iter().take_while(|line| !line.is_empty()) {
    let (rule_num, rule_text) = rule_line.split(':').collect_tuple().unwrap();
    let rule_num = rule_num.parse::<usize>().unwrap();
    let rule_uuid = *mapping.entry(rule_num).or_insert_with(Uuid::new_v4);

    let current_rules = rule_text
      .split('|')
      .map(|rule_chunk| Rule {
        contents: rule_chunk
          .split_whitespace()
          .map(|r| match schar_match.captures(r) {
            Some(cap) => Right(cap[1].chars().nth(0).unwrap()),
            None => Left(
              *mapping
                .entry(r.parse::<usize>().unwrap())
                .or_insert_with(Uuid::new_v4),
            ),
          })
          .collect_vec(),
      })
      .collect_vec();

    rules.insert(rule_uuid, current_rules);
  }

  let toplevel = Uuid::new_v4();
  rules.insert(
    toplevel,
    vec![Rule {
      contents: vec![Left(*mapping.get(&0).unwrap())],
    }],
  );

  let passing = lines
    .iter()
    .skip_while(|line| !line.is_empty())
    .skip(1)
    .filter(|line| parse(&rules, (toplevel, 0), line))
    .count();

  println!("Part 1: {}", passing);

  rules.insert(
    mapping[&8],
    vec![
      Rule {
        contents: vec![Left(mapping[&42])],
      },
      Rule {
        contents: vec![Left(mapping[&42]), Left(mapping[&8])],
      },
    ],
  );
  rules.insert(
    mapping[&11],
    vec![
      Rule {
        contents: vec![Left(mapping[&42]), Left(mapping[&31])],
      },
      Rule {
        contents: vec![
          Left(mapping[&42]),
          Left(mapping[&11]),
          Left(mapping[&31]),
        ],
      },
    ],
  );

  let passing = lines
    .iter()
    .skip_while(|line| !line.is_empty())
    .skip(1)
    .filter(|line| parse(&rules, (toplevel, 0), line))
    .count();

  println!("Part 2: {}", passing);
}

fn parse(
  rules: &HashMap<Uuid, Vec<Rule>>,
  toplevel: (Uuid, usize),
  matching: &str,
) -> bool {
  #[allow(non_snake_case)]
  let mut S: Vec<OrderSet<PState>> = vec![OrderSet::new(); matching.len() + 1];

  let (toplvl_uuid, toplvl_rule) = rules
    .get_key_value(&toplevel.0)
    .map(|(uuid, ruleset)| (uuid, ruleset.get(toplevel.1).unwrap()))
    .unwrap();
  S[0].insert(PState {
    rule_uuid: toplvl_uuid,
    rule_idx: toplevel.1,
    rule: toplvl_rule,
    pos: 0,
    origin: 0,
  });

  for (i, matching_char) in matching
    .chars()
    .map(Some)
    .enumerate()
    .chain([(matching.len(), None)])
  {
    let mut pi = 0;
    while pi < S[i].len() {
      let pstate = S[i][pi];
      if pstate.pos == pstate.rule.contents.len() {
        // COMPLETER
        let completed = S[pstate.origin]
          .iter()
          .filter(|old_pstate| {
            old_pstate
              .rule
              .contents
              .get(old_pstate.pos)
              .is_some_and(|test| {
                test.left().is_some_and(|uuid| uuid == *pstate.rule_uuid)
              })
          })
          .map(|old_pstate| PState {
            pos: old_pstate.pos + 1,
            ..*old_pstate
          })
          .collect_vec();

        S[i].extend(completed);
      } else {
        match pstate.rule.contents[pstate.pos] {
          Left(uuid) => {
            // PREDICTOR
            for (rule_uuid, rule_idx, rule) in rules
              .get_key_value(&uuid)
              .map(|(uuid, ruleset)| {
                ruleset
                  .iter()
                  .enumerate()
                  .map(move |(idx, rule)| (uuid, idx, rule))
              })
              .unwrap()
            {
              S[i].insert(PState {
                rule_uuid,
                rule_idx,
                rule,
                pos: 0,
                origin: i,
              });
            }
          }
          Right(c) => {
            // SCANNER
            if matching_char.is_some_and(|mchar| mchar == c)
              && pstate.origin < matching.len()
            {
              S[i + 1].insert(PState {
                pos: pstate.pos + 1,
                ..pstate
              });
            } else {
            }
          }
        }
      }

      pi += 1;
    }
  }

  S[matching.len()].contains(&PState {
    rule_uuid: toplvl_uuid,
    rule_idx: toplevel.1,
    rule: toplvl_rule,
    pos: 1,
    origin: 0,
  })
}
