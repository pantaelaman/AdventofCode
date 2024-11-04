use itertools::Itertools;

fn main() {
  let argument = std::env::args().nth(1).unwrap();
  let buf = argument
    .chars()
    .map(|c| (c as u8 - b'0') as usize)
    .collect_vec();
  let target_recipe = argument.parse::<usize>().unwrap();

  let mut recipes = vec![3, 7];
  let mut elves = [0, 1];
  let mut pattern_seen_at = None;
  while recipes.len() < target_recipe + 10 || pattern_seen_at.is_none() {
    for new_recipe in
      format!("{}", elves.iter().map(|i| recipes[*i]).sum::<usize>())
        .chars()
        .map(|c| (c as u8 - b'0') as usize)
    {
      recipes.push(new_recipe);
      if pattern_seen_at.is_none()
        && recipes
          .len()
          .checked_sub(buf.len())
          .map(|i| {
            // println!("{:?}", &recipes[i..]);
            recipes[i..].iter().eq(buf.iter())
          })
          .unwrap_or_default()
      {
        pattern_seen_at = Some(recipes.len() - buf.len());
      }
    }
    for elf in elves.iter_mut() {
      *elf = (*elf + recipes[*elf] + 1) % recipes.len();
    }
  }

  println!(
    "Part 1: {}",
    recipes[target_recipe..target_recipe + 10]
      .iter()
      .map(|n| (*n as u8 + b'0') as char)
      .collect::<String>()
  );
  println!("Part 2: {}", pattern_seen_at.unwrap());
}
