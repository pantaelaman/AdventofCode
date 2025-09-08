use std::{
  collections::{HashMap, HashSet},
  io::stdin,
};

use itertools::Itertools;

struct Food<'s> {
  ingredients: HashSet<&'s str>,
  allergens: HashSet<&'s str>,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let mut all_ingredients = HashSet::new();

  let foods = lines
    .iter()
    .map(|line| {
      let mut chunks = line.split(" (contains ");
      let ingredients = chunks
        .next()
        .unwrap()
        .split_whitespace()
        .inspect(|ingredient| {
          all_ingredients.insert(*ingredient);
        })
        .collect();
      let allergens = chunks
        .next()
        .unwrap()
        .strip_suffix(")")
        .unwrap()
        .split(", ")
        .collect();
      Food {
        ingredients,
        allergens,
      }
    })
    .collect_vec();

  let allergens: HashMap<&str, HashSet<&str>> =
    foods.iter().fold(HashMap::new(), |mut acc, food| {
      for allergen in food.allergens.iter() {
        match acc.get_mut(allergen) {
          Some(possibles) => {
            *possibles =
              possibles.intersection(&food.ingredients).copied().collect()
          }
          None => {
            acc.insert(allergen, food.ingredients.clone());
          }
        }
      }
      acc
    });

  let possible_allergenic_ingredients =
    allergens.values().flat_map(|v| v.iter()).copied().collect();

  let safe_ingredients: HashSet<&str> = all_ingredients
    .difference(&possible_allergenic_ingredients)
    .copied()
    .collect();

  let safe_ingredient_appearances = foods
    .iter()
    .flat_map(|food| food.ingredients.iter())
    .filter(|ingredient| safe_ingredients.contains(*ingredient))
    .count();

  println!("Part 1: {}", safe_ingredient_appearances);

  // ingredients and allergens are a one-to-one mapping (assumption, seems plausible according to the problem text)

  let mut allergens = allergens;
  let mut solved_allergens = HashMap::new();
  while !allergens.is_empty() {
    let recently_solved = allergens
      .extract_if(|_, list| list.len() == 1)
      .map(|(allergen, ingredients)| {
        (allergen, ingredients.iter().copied().exactly_one().unwrap())
      })
      .collect_vec();

    for ingredient in recently_solved.iter().map(|(_, i)| i) {
      for allergen_ingredients in allergens.values_mut() {
        allergen_ingredients.remove(ingredient); // already taken!
      }
    }

    solved_allergens.extend(recently_solved);
  }

  let canonical = solved_allergens
    .iter()
    .sorted_by_key(|(allergen, _)| *allergen)
    .map(|(_, ingredient)| ingredient)
    .join(",");

  println!("Part 2: {}", canonical);
}
