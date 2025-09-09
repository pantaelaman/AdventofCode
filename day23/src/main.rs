use std::{collections::HashMap, io::stdin, iter::successors};

use itertools::Itertools;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let init = lines[0]
    .chars()
    .map(|c| c.to_digit(10).unwrap())
    .collect_vec();

  let mut cups: HashMap<u32, u32> = HashMap::new();
  cups.extend(init.iter().copied().circular_tuple_windows::<(_, _)>());

  successors(Some(init[0]), |current| {
    Some(do_move(&mut cups, *current, 9))
  })
  .nth(99);

  let p1 = successors(Some(1), |c| Some(cups[c]))
    .skip(1)
    .take_while(|c| *c != 1)
    .map(|c| char::from_digit(c, 10).unwrap())
    .collect::<String>();
  println!("Part 1: {}", p1);

  let mut cups: HashMap<u32, u32> = HashMap::with_capacity(1000000);
  cups.extend(
    init
      .iter()
      .copied()
      .chain(10..)
      .take(1000000)
      .chain([init[0]])
      .tuple_windows::<(_, _)>(),
  );

  successors(Some(init[0]), |current| {
    Some(do_move(&mut cups, *current, 1000000))
  })
  .nth(9999999);

  let p2 = successors(Some(1), |c| Some(cups[c]))
    .skip(1)
    .take(2)
    .map(|v| v as u128)
    .product::<u128>();
  println!("Part 2: {}", p2);
}

fn print_slice(cups: &HashMap<u32, u32>, start: u32) {
  print!("[{start}, ");
  for cup in successors(Some(start), |c| Some(cups[&c]))
    .skip(1)
    .take_while(|c| *c != start)
  {
    print!("{cup}, ");
  }
  println!("]");
}

fn do_move(cups: &mut HashMap<u32, u32>, current: u32, max_cup: u32) -> u32 {
  let taken = successors(Some(current), |c| Some(cups[&c]))
    .skip(1)
    .take(3)
    .collect_vec();
  let next = cups[taken.last().unwrap()];
  *cups.get_mut(&current).unwrap() = next;

  let dest = successors(Some(current), |c| {
    Some(match c {
      1 => max_cup,
      _ => c - 1,
    })
  })
  .skip(1)
  .find(|c| !taken.contains(c))
  .unwrap();

  let after_dest = cups[&dest];
  *cups.get_mut(taken.last().unwrap()).unwrap() = after_dest;
  *cups.get_mut(&dest).unwrap() = *taken.first().unwrap();

  next
}
