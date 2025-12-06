use std::io::stdin;

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let (ranges, ingredients) =
    lines.split(|line| line.is_empty()).collect_tuple().unwrap();

  let ranges = ranges
    .iter()
    .map(|line| {
      let (start, end) = line
        .split('-')
        .map(|num| num.parse::<usize>().unwrap())
        .collect_tuple()
        .unwrap();
      start..=end
    })
    .collect_vec();

  let ingredients = ingredients
    .iter()
    .map(|ing| ing.parse::<usize>().unwrap())
    .collect_vec();

  let num_fresh = ingredients
    .iter()
    .filter(|ingredient| ranges.iter().any(|range| range.contains(*ingredient)))
    .count();

  println!("Part 1: {}", num_fresh);

  let ranges = {
    let mut ranges = ranges;
    ranges.sort_unstable_by_key(|r| *r.start());
    ranges
  };

  let num_fresh = ranges
    .into_iter()
    .peekable()
    .batching(|ranges| {
      let mut acc = ranges.next()?;

      while let Some(next) = ranges.next_if(|next| acc.contains(next.start())) {
        acc = *acc.start()..=std::cmp::max(*acc.end(), *next.end());
      }

      Some(acc)
    })
    //.inspect(|range| println!("{:?}", range))
    .map(|range| range.end() - range.start() + 1)
    .sum::<usize>();

  println!("Part 2: {}", num_fresh);
}
