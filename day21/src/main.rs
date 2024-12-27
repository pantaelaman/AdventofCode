#![feature(int_roundings)]
use std::io::stdin;

use itertools::Itertools;
use regex::Regex;

static WEAPONS: phf::Map<&'static str, [i32; 3]> = phf::phf_map! {
  "Dagger" =>       [ 8,     4,      0],
  "Shortsword" =>   [10,     5,      0],
  "Warhammer" =>    [25,     6,      0],
  "Longsword" =>    [40,     7,      0],
  "Greataxe" =>     [74,     8,      0],
};

static ARMOURS: phf::Map<&'static str, [i32; 3]> = phf::phf_map! {
  "Naked" =>       [ 0,      0,      0],
  "Leather" =>     [ 13,     0,      1],
  "Chainmail" =>   [ 31,     0,      2],
  "Splintmail" =>  [ 53,     0,      3],
  "Bandedmail" =>  [ 75,     0,      4],
  "Platemail" =>   [102,     0,      5],
};

static RINGS: phf::Map<&'static str, [i32; 3]> = phf::phf_map! {
  "L Empty" =>     [  0,     0,      0],
  "R Empty" =>     [  0,     0,      0],
  "Damage +1" =>   [ 25,     1,      0],
  "Damage +2" =>   [ 50,     2,      0],
  "Damage +3" =>   [100,     3,      0],
  "Defense +1" =>  [ 20,     0,      1],
  "Defense +2" =>  [ 40,     0,      2],
  "Defense +3" =>  [ 80,     0,      3],
};

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");
  let regex = Regex::new(r"\d+").unwrap();
  let (boss_hp, boss_damage, boss_armour): (i32, i32, i32) = regex
    .find_iter(&contents)
    .map(|m| m.as_str().parse::<i32>().unwrap())
    .collect_tuple()
    .unwrap();

  let (min_cost, max_cost) = WEAPONS
    .values()
    .cartesian_product(ARMOURS.values())
    .cartesian_product(RINGS.values().tuple_combinations())
    .map(collapse_gearset)
    .fold(
      (i32::MAX, i32::MIN),
      |(min_cost, max_cost), (cost, stats)| {
        if is_win(100, &stats, boss_hp, &[boss_damage, boss_armour]) {
          (min_cost.min(cost), max_cost)
        } else {
          (min_cost, max_cost.max(cost))
        }
      },
    );

  println!("Test: {}", is_win(8, &[5, 5], 12, &[7, 2]));

  println!("Part 1: {min_cost}");
  println!("Part 1: {max_cost}");
}

fn collapse_gearset(
  (
    ([wcost, wdmg, wdef], [acost, admg, adef]),
    ([lcost, ldmg, ldef], [rcost, rdmg, rdef]),
  ): ((&[i32; 3], &[i32; 3]), (&[i32; 3], &[i32; 3])),
) -> (i32, [i32; 2]) {
  let cost = wcost + acost + lcost + rcost;
  let stats = [wdmg + admg + ldmg + rdmg, wdef + adef + ldef + rdef];
  (cost, stats)
}

fn is_win(
  hp: i32,
  stats: &[i32; 2],
  boss_hp: i32,
  boss_stats: &[i32; 2],
) -> bool {
  let atk = (stats[0] - boss_stats[1]).max(1); // player -> boss
  let dmg = (boss_stats[0] - stats[1]).max(1); // boss -> player
  (boss_hp.div_ceil(atk) - 1) * dmg < hp
}
