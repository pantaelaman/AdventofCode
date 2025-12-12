use std::io::stdin;

use itertools::Itertools;
use nom::{
  character::{self, complete::multispace0, one_of},
  multi::{many1, separated_list1},
  sequence::{separated_pair, terminated},
  IResult, Parser,
};

#[derive(Debug)]
struct Present {
  shape: [[bool; 3]; 3],
  area: usize,
}

impl Present {
  pub fn new(shape: [[bool; 3]; 3]) -> Self {
    Self {
      shape,
      area: shape
        .iter()
        .flat_map(|row| row.iter())
        .filter(|s| **s)
        .count(),
    }
  }
}

#[derive(Debug)]
struct Region {
  width: usize,
  height: usize,
  counts: Vec<usize>,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");

  let Ok((_, (presents, mut regions))) = input(&contents) else {
    unimplemented!("unparseable input")
  };

  let mut trivial_successes = 0;
  let mut trivial_failures = 0;

  regions.retain(|region| {
    let area = region.width * region.height;
    let required_area: usize = region
      .counts
      .iter()
      .zip(presents.iter())
      .map(|(count, present)| count * present.area)
      .sum();

    if required_area > area {
      trivial_failures += 1;
      return false;
    }

    let bwidth = region.width / 3;
    let bheight = region.height / 3;
    if bwidth * bheight >= region.counts.iter().sum() {
      trivial_successes += 1;
      return false;
    }

    true
  });

  println!("{regions:?}");
  println!("Successes: {trivial_successes}");
  println!("Failures: {trivial_failures}");
}

fn input(input: &str) -> IResult<&str, (Vec<Present>, Vec<Region>)> {
  let (input, presents) = many1(present.map(|(_, p)| p)).parse(input)?;
  let (input, regions) = many1(region).parse(input)?;

  Ok((input, (presents, regions)))
}

fn present(input: &str) -> IResult<&str, (usize, Present)> {
  let (input, index) =
    terminated(character::complete::usize, character::char(':'))
      .parse(input)?;

  fn shape_char(input: &str) -> IResult<&str, bool> {
    one_of(".#").map(|c| c == '#').parse(input)
  }
  fn row(input: &str) -> IResult<&str, (bool, bool, bool)> {
    terminated((shape_char, shape_char, shape_char), multispace0).parse(input)
  }
  let (input, _) = multispace0(input)?;

  let (input, st) = (row, row, row).parse(input)?;

  let present = Present::new([
    [st.0 .0, st.0 .1, st.0 .2],
    [st.1 .0, st.1 .1, st.1 .2],
    [st.2 .0, st.2 .1, st.2 .2],
  ]);

  Ok((input, (index, present)))
}

fn region(input: &str) -> IResult<&str, Region> {
  let (input, (width, height)) = terminated(
    separated_pair(
      character::complete::usize,
      character::char('x'),
      character::complete::usize,
    ),
    character::char(':'),
  )
  .parse(input)?;

  let (input, _) = multispace0(input)?;
  let (input, counts) = terminated(
    separated_list1(character::complete::space1, character::complete::usize),
    character::complete::newline,
  )
  .parse(input)?;

  let region = Region {
    width,
    height,
    counts,
  };

  Ok((input, region))
}
