use std::io::stdin;

use itertools::Itertools;
use regex::Regex;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let target_regex =
    Regex::new(r"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)").unwrap();

  let caps = target_regex.captures(&lines[0]).unwrap();
  let (xmin, xmax, ymin, ymax) = caps
    .iter()
    .skip(1)
    .map(|c| c.unwrap().as_str().parse::<i32>().unwrap())
    .collect_tuple()
    .unwrap();

  let (xrange, yrange) = (xmin..=xmax, ymin..=ymax);

  let vx0min = (((1. + 8. * xmin as f64).sqrt() - 1.) / 2.).ceil() as i32;
  let vx0max = xmax;

  let vx0_range = vx0min..=vx0max; // anything in here MAY be a valid solution
  let vy0_range = ymin..=-ymin; // anything in here MAY be a valid solution

  let solutions = vx0_range
    .cartesian_product(vy0_range)
    .filter(|v0| {
      let (mut vx, mut vy) = v0;
      let (mut x, mut y) = (0, 0);
      loop {
        x += vx;
        y += vy;

        if xrange.contains(&x) && yrange.contains(&y) {
          break true;
        } else if x > xmax || y < ymin {
          break false;
        }

        if vx > 0 {
          vx -= 1;
        }
        vy -= 1;
      }
    })
    .collect_vec();

  let highest_y = solutions
    .iter()
    .map(|(_, vy0)| (2 * vy0 + 1).pow(2) / 8)
    .max()
    .unwrap();
  println!("Part 1: {}", highest_y);

  println!("Part 2: {}", solutions.len());
}
