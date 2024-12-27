use std::{
  cmp::Reverse,
  collections::{BinaryHeap, HashMap},
  io::stdin,
};

use bitfields::bitfield;
use itertools::Itertools;
use regex::Regex;

struct MagicMissile;
struct Drain;
struct Shield;
struct Poison;
struct Recharge;

impl Spell for MagicMissile {
  fn mana_cost(&self) -> u32 {
    53
  }
  fn effect(&self, state: &mut State) -> bool {
    state.attack(4)
  }
  fn runnable(&self, state: &State) -> bool {
    state.mana >= 53
  }
}

impl Spell for Drain {
  fn mana_cost(&self) -> u32 {
    73
  }
  fn effect(&self, state: &mut State) -> bool {
    state.hp += 2;
    state.attack(2)
  }
}

impl Spell for Shield {
  fn mana_cost(&self) -> u32 {
    113
  }
  fn effect(&self, state: &mut State) -> bool {
    state.effects.set_shield(6);
    true
  }
  fn runnable(&self, state: &State) -> bool {
    state.effects.shield() == 0
  }
}

impl Spell for Poison {
  fn mana_cost(&self) -> u32 {
    173
  }
  fn effect(&self, state: &mut State) -> bool {
    state.effects.set_poison(6);
    true
  }
  fn runnable(&self, state: &State) -> bool {
    state.effects.poison() == 0
  }
}

impl Spell for Recharge {
  fn mana_cost(&self) -> u32 {
    229
  }
  fn effect(&self, state: &mut State) -> bool {
    state.effects.set_recharge(5);
    true
  }
  fn runnable(&self, state: &State) -> bool {
    state.effects.recharge() == 0
  }
}

const SPELLS: &'static [&'static dyn Spell] =
  &[&MagicMissile, &Drain, &Shield, &Poison, &Recharge];

trait Spell {
  fn mana_cost(&self) -> u32;
  fn effect(&self, state: &mut State) -> bool;
  fn runnable(&self, state: &State) -> bool {
    true
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct State {
  mana: u32,
  hp: i32,
  boss: i32,
  effects: EffectState,
}

impl State {
  fn boss_attack(&mut self, boss_atk: i32) -> bool {
    let armour = 7 * (self.effects.shield() > 0) as i32;
    self.hp -= (boss_atk - armour).max(1);
    self.hp > 0
  }

  fn attack(&mut self, attack: i32) -> bool {
    self.boss -= attack;
    self.boss > 0
  }

  fn tick(&mut self) -> bool {
    let shield = self.effects.shield();
    let poison = self.effects.poison();
    let recharge = self.effects.recharge();
    if shield > 0 {
      self.effects.set_shield(shield - 1);
    }
    if poison > 0 {
      self.boss -= 3;
      self.effects.set_poison(poison - 1);
    }
    if recharge > 0 {
      self.mana += 101;
      self.effects.set_recharge(recharge - 1);
    }
    self.boss > 0
  }
}

#[bitfield(u16)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct EffectState {
  #[bits(3)]
  shield: u8,
  #[bits(3)]
  poison: u8,
  #[bits(3)]
  recharge: u8,
  #[bits(7)]
  _padding: u8,
}

struct PQEntry<P, I>(pub P, pub I);

impl<P: Eq, I> PartialEq for PQEntry<P, I> {
  fn eq(&self, other: &Self) -> bool {
    self.0.eq(&other.0)
  }
}

impl<P: Eq, I> Eq for PQEntry<P, I> {}

impl<P: Ord, I> PartialOrd for PQEntry<P, I> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.0.cmp(&other.0))
  }
}

impl<P: Ord, I> Ord for PQEntry<P, I> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.0.cmp(&other.0)
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");
  let regex = Regex::new(r"\d+").unwrap();
  let (boss_hp, boss_atk) = regex
    .find_iter(&contents)
    .map(|m| m.as_str().parse::<i32>().unwrap())
    .collect_tuple()
    .unwrap();

  let min_mana = dijkstra(boss_atk, boss_hp).unwrap();
  println!("Part 1: {min_mana}");
  let min_mana = dijkstra_decay(boss_atk, boss_hp).unwrap();
  println!("Part 2: {min_mana}");
}

fn dijkstra(boss_atk: i32, boss_hp: i32) -> Option<u32> {
  let mut frontier: BinaryHeap<Reverse<PQEntry<u32, State>>> =
    BinaryHeap::new();
  let mut costs: HashMap<State, u32> = HashMap::new();
  let start = State {
    mana: 500,
    hp: 50,
    boss: boss_hp,
    effects: EffectState::default(),
  };
  costs.insert(start.clone(), 0);
  frontier.push(Reverse(PQEntry(0, start)));

  while let Some(Reverse(PQEntry(spent_mana, mut state))) = frontier.pop() {
    if state.boss <= 0 {
      return Some(spent_mana);
    }
    if state.hp <= 0 {
      continue;
    }
    if !state.tick() {
      return Some(spent_mana);
    }
    let successors = SPELLS
      .iter()
      .copied()
      .filter(|s| s.mana_cost() <= state.mana && s.runnable(&state));
    for successor in successors {
      let mut next_state = state.clone();
      let next_spent_mana = spent_mana + successor.mana_cost();
      next_state.mana -= successor.mana_cost();
      successor.effect(&mut next_state);
      next_state.tick();
      next_state.boss_attack(boss_atk);
      if costs
        .get(&next_state)
        .map_or(true, |c| &next_spent_mana < c)
      {
        costs.insert(next_state.clone(), next_spent_mana);
        frontier.push(Reverse(PQEntry(next_spent_mana, next_state)));
      }
    }
  }

  None
}

fn dijkstra_decay(boss_atk: i32, boss_hp: i32) -> Option<u32> {
  let mut frontier: BinaryHeap<Reverse<PQEntry<u32, State>>> =
    BinaryHeap::new();
  let mut costs: HashMap<State, u32> = HashMap::new();
  let start = State {
    mana: 500,
    hp: 50,
    boss: boss_hp,
    effects: EffectState::default(),
  };
  costs.insert(start.clone(), 0);
  frontier.push(Reverse(PQEntry(0, start)));

  while let Some(Reverse(PQEntry(spent_mana, mut state))) = frontier.pop() {
    if state.boss <= 0 {
      return Some(spent_mana);
    }
    if state.hp <= 0 {
      continue;
    }
    if !state.tick() {
      return Some(spent_mana);
    }
    let successors = SPELLS
      .iter()
      .copied()
      .filter(|s| s.mana_cost() <= state.mana && s.runnable(&state));
    for successor in successors {
      let mut next_state = state.clone();
      let next_spent_mana = spent_mana + successor.mana_cost();
      next_state.hp -= 1;
      if next_state.hp <= 0 {
        continue;
      }
      next_state.mana -= successor.mana_cost();
      successor.effect(&mut next_state);
      next_state.tick();
      next_state.boss_attack(boss_atk);
      if costs
        .get(&next_state)
        .map_or(true, |c| &next_spent_mana < c)
      {
        costs.insert(next_state.clone(), next_spent_mana);
        frontier.push(Reverse(PQEntry(next_spent_mana, next_state)));
      }
    }
  }

  None
}
