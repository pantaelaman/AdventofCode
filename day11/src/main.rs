use std::collections::HashMap;

use itertools::Itertools;

const KERNEL: [(i64, i64); 9] = [
  (0, 0),
  (1, 0),
  (1, 1),
  (0, 1),
  (-1, 1),
  (-1, 0),
  (-1, -1),
  (0, -1),
  (1, -1),
];

fn main() {
  let serial = std::env::args().nth(1).unwrap().parse::<i64>().unwrap();

  let power_grid: HashMap<(i64, i64), i64> = (1..=300)
    .cartesian_product(1..=300)
    .map(|(x, y)| {
      let rack_id = x + 10;
      // I can feel modular arithmetic coming -- not today, sadly
      let big_num = ((rack_id * y) + serial) * rack_id;
      let hundreds = (big_num / 100) % 10;
      ((x, y), hundreds - 5)
    })
    .collect();

  let summed_area_table: [[i64; 301]; 301] = {
    let mut summed_area_table = [[0; 301]; 301];
    for (x, y) in (1..=300).cartesian_product(1..=300) {
      summed_area_table[y][x] = power_grid
        .get(&(x as i64, y as i64))
        .copied()
        .unwrap_or_default()
        + summed_area_table[y - 1][x]
        + summed_area_table[y][x - 1]
        - summed_area_table[y - 1][x - 1];
    }
    summed_area_table
  };

  let mut widths = (3..300).map(|size| {
    (1..=(300 - size))
      .cartesian_product(1..=(300 - size))
      .map(move |(x, y)| {
        (
          (x, y),
          size,
          summed_area_table[y - 1][x - 1]
            + summed_area_table[y + size - 1][x + size - 1]
            - summed_area_table[y - 1][x + size - 1]
            - summed_area_table[y + size - 1][x - 1],
        )
      })
      .max_by_key(|(_, _, level)| *level)
      .unwrap()
  });

  let ((x1, y1), s1, _) = widths.next().unwrap();
  let ((x2, y2), s2, _) = widths.max_by_key(|(_, _, level)| *level).unwrap();

  println!("Part 1: {},{},{}", x1, y1, s1);
  println!("Part 2: {},{},{}", x2, y2, s2);
}
