use std::{io::stdin, ops::RangeInclusive};

use itertools::Itertools;

fn area((ax, ay): (i64, i64), (bx, by): (i64, i64)) -> u64 {
  (ax.abs_diff(bx) + 1) * (ay.abs_diff(by) + 1)
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let tiles: Vec<(i64, i64)> = lines
    .iter()
    .map(|line| {
      line
        .split(',')
        .map(|n| n.parse::<i64>().unwrap())
        .collect_tuple()
        .unwrap()
    })
    .collect_vec();

  let mut rects_by_size = tiles
    .iter()
    .tuple_combinations()
    .map(|(l, r)| ((l, r), area(*l, *r)))
    .collect_vec();

  rects_by_size.sort_by_key(|(_, a)| *a);
  rects_by_size.reverse();

  println!("Part 1: {}", rects_by_size[0].1);

  let (mut cols, mut rows) = tiles.iter().circular_tuple_windows().fold(
    (Vec::new(), Vec::new()),
    |(mut cols, mut rows), (l, r)| {
      if l.0 == r.0 {
        cols.push((l.0, (r.1 - l.1).signum(), l.1.min(r.1)..=l.1.max(r.1)));
      } else {
        rows.push((l.1, (r.0 - l.0).signum(), l.0.min(r.0)..=l.0.max(r.0)));
      }

      (cols, rows)
    },
  );

  cols.sort_unstable_by_key(|(x, _, _)| *x);
  rows.sort_unstable_by_key(|(y, _, _)| *y);

  if cols[0].1 < 0 {
    cols.iter_mut().for_each(|(_, ref mut d, _)| *d = -*d);
  }
  if rows[0].1 < 0 {
    rows.iter_mut().for_each(|(_, ref mut d, _)| *d = -*d);
  }

  for ((l, r), a) in rects_by_size {
    let (top, bottom) = (l.1.min(r.1), l.1.max(r.1));
    let (left, right) = (l.0.min(r.0), l.0.max(r.0));

    let clear_top = contained(top, left, right, cols.iter());
    let clear_bottom = contained(bottom, left, right, cols.iter());
    let clear_left = contained(left, top, bottom, rows.iter());
    let clear_right = contained(right, top, bottom, rows.iter());

    if clear_top && clear_bottom && clear_left && clear_right {
      println!("Part 2: {}", a);

      break;
    }
  }
}

fn contained<'a>(
  range_guard: i64,
  first: i64,
  last: i64,
  bars: impl Iterator<Item = &'a (i64, i64, RangeInclusive<i64>)>,
) -> bool {
  bars
    .filter(|(_, _, range)| range.contains(&range_guard))
    .batching(|it| {
      let first = it.next()?;

      while let Some(next) = it.next() {
        if next.1 != first.1 {
          return Some((first, next));
        }
      }

      None
    })
    .any(|((f, _, _), (l, _, _))| *f <= first && *l >= last)
}
