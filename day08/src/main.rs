use std::io::stdin;

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  const WIDTH: usize = 50;
  const HEIGHT: usize = 6;
  let mut grid = [[false; WIDTH]; HEIGHT];

  let nums = Regex::new(r"\d+").unwrap();

  for line in lines {
    let mut chunks = line.split_whitespace();
    match chunks.next().unwrap() {
      "rect" => {
        let (w, h) = chunks
          .next()
          .unwrap()
          .split("x")
          .map(|v| v.parse::<usize>().unwrap())
          .collect_tuple()
          .unwrap();
        for (x, y) in (0..w).cartesian_product(0..h) {
          grid[y][x] = true;
        }
      }
      "rotate" => match chunks.next().unwrap() {
        "row" => {
          let (row, n) = nums
            .find_iter(&chunks.join(" "))
            .map(|m| m.as_str().parse::<usize>().unwrap())
            .collect_tuple()
            .unwrap();
          grid[row].rotate_right(n);
        }
        "column" => {
          let (col, n) = nums
            .find_iter(&chunks.join(" "))
            .map(|m| m.as_str().parse::<usize>().unwrap())
            .collect_tuple()
            .unwrap();
          for (y, v) in (0..HEIGHT)
            .map(|y| grid[y][col])
            .collect_vec()
            .into_iter()
            .enumerate()
          {
            grid[(y + n) % HEIGHT][col] = v;
          }
        }
        _ => unimplemented!(),
      },
      _ => unimplemented!(),
    }
  }

  let n_lit = grid
    .iter()
    .flat_map(|row| row.iter().copied())
    .filter(|v| *v)
    .count();
  println!("Part 1: {n_lit}");
  print_grid(&grid);
}

fn print_grid<const W: usize, const H: usize>(grid: &[[bool; W]; H]) {
  for y in 0..H {
    for x in 0..W {
      print!("{}", if grid[y][x] { '#' } else { '.' });
    }
    println!();
  }
}
