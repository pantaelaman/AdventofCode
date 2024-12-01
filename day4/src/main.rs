use itertools::Itertools;

fn main() {
  let (low, high) = std::env::args()
    .nth(1)
    .unwrap()
    .split('-')
    .map(|p| p.parse::<usize>().unwrap())
    .collect_tuple()
    .unwrap();

  let num_pwords = (low..=high)
    .filter(|num| {
      let strver = format!("{}", num);
      strver.chars().tuple_windows().all(|(l, r)| r >= l)
        && strver
          .chars()
          .chunk_by(|v| *v)
          .into_iter()
          .any(|(_, g)| g.count() >= 2)
    })
    .count();

  println!("Part 1: {}", num_pwords);

  let num_pwords = (low..=high)
    .filter(|num| {
      let strver = format!("{}", num);
      strver.chars().tuple_windows().all(|(l, r)| r >= l)
        && strver
          .chars()
          .chunk_by(|v| *v)
          .into_iter()
          .any(|(_, g)| g.count() == 2)
    })
    .count();

  println!("Part 2: {}", num_pwords);
}
