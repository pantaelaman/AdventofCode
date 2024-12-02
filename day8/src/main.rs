use itertools::Itertools;
use std::{fs::File, io::Read};

fn main() {
  let mut critical_args = std::env::args().skip(1).take(3);
  let mut file = File::open(critical_args.next().unwrap()).unwrap();
  let (width, height) = critical_args
    .map(|s| s.parse::<usize>().unwrap())
    .collect_tuple()
    .unwrap();
  let size = width * height;
  let mut buf = String::new();
  file.read_to_string(&mut buf).unwrap();

  let layers = buf
    .trim()
    .chars()
    .map(|c| c.to_digit(10).unwrap())
    .chunks(size)
    .into_iter()
    .map(|chunk| chunk.collect_vec())
    .collect_vec();

  let (_, ones, twos) = layers
    .iter()
    .map(|chunk| {
      chunk
        .iter()
        .fold((0, 0, 0), |(zeroes, ones, twos), digit| match digit {
          0 => (zeroes + 1, ones, twos),
          1 => (zeroes, ones + 1, twos),
          2 => (zeroes, ones, twos + 1),
          _ => (zeroes, ones, twos),
        })
    })
    .min_by_key(|(zeroes, _, _)| *zeroes)
    .unwrap();

  println!("Part 1: {}", ones * twos);

  println!("Part 2: ");
  for y in 0..height {
    for x in 0..width {
      print!(
        "{}",
        if get_pixel(width, x, y, 0, &layers) {
          '#'
        } else {
          ' '
        }
      );
    }
    println!();
  }
}

fn get_pixel(
  width: usize,
  x: usize,
  y: usize,
  layer: usize,
  layers: &Vec<Vec<u32>>,
) -> bool {
  match layers[layer][x + y * width] {
    0 => false,
    1 => true,
    2 => get_pixel(width, x, y, layer + 1, layers),
    _ => unreachable!(),
  }
}
