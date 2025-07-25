#![feature(iter_map_windows)]
use std::{
  collections::{HashMap, HashSet, VecDeque},
  io::stdin,
};

use itertools::Itertools;
use md5::Digest;

struct KeyGen {
  salt: String,
  index: usize,
}

impl KeyGen {
  fn new(salt: String) -> Self {
    KeyGen { salt, index: 0 }
  }
}

impl Iterator for KeyGen {
  type Item = Digest;

  fn next(&mut self) -> Option<Self::Item> {
    let hash = md5::compute(format!("{}{}", self.salt, self.index));
    self.index += 1;
    Some(hash)
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");
  let keygen = KeyGen::new(contents);

  let mut queued: HashMap<u8, Vec<Digest>> = HashMap::new();
  let mut generated: Vec<Digest> = Vec::new();
  for (i, key) in keygen.enumerate() {
    if let Some(sl) = key.windows(5).find(|sl| sl.iter().all_equal()) {
      queued.entry(sl[0]).or_default().push(key);
    }
  }
}
