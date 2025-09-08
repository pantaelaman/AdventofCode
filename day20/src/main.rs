use std::{collections::HashMap, io::stdin};

use itertools::Itertools;
use ndarray::{arr2, s, Array2, Axis};

#[derive(Clone)]
struct Tile {
  data: Array2<bool>,
}

impl Tile {
  fn get_border(&self, border: u8) -> u16 {
    let border = border % 4; // safety
    let axis = if border % 2 == 0 { Axis(1) } else { Axis(0) };
    let index = if border == 1 || border == 2 { 9 } else { 0 };
    let border_val =
      self
        .data
        .index_axis(axis, index)
        .iter()
        .fold(0, |mut acc, v| {
          acc <<= 1;
          if *v {
            acc |= 1;
          }
          acc
        });

    if border >= 2 {
      flip_border(border_val)
    } else {
      border_val
    }
  }

  fn flip_h(&mut self) {
    self.data.invert_axis(Axis(0));
  }

  fn flip_v(&mut self) {
    self.data.invert_axis(Axis(1));
  }

  fn rotate(&mut self, amount: u8) {
    match amount % 4 {
      0 => {}
      1 => {
        self.data.swap_axes(0, 1);
        self.data.invert_axis(Axis(0));
      }
      2 => {
        self.data.invert_axis(Axis(0));
        self.data.invert_axis(Axis(1));
      }
      3 => {
        self.data.swap_axes(0, 1);
        self.data.invert_axis(Axis(1));
      }
      _ => unreachable!(),
    }
  }
}

impl std::fmt::Display for Tile {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for row in self.data.lanes(Axis(0)) {
      for b in row.iter() {
        if *b {
          write!(f, "#")?;
        } else {
          write!(f, ".")?;
        }
      }
      writeln!(f, "")?;
    }
    Ok(())
  }
}

fn flip_border(border: u16) -> u16 {
  border.reverse_bits() >> 6
}

fn invert_rotation(rot: u8) -> u8 {
  4 - rot
}

#[derive(Debug, Clone, Copy)]
struct TileAlignment {
  tile: usize,
  flip: bool,
  rot: u8, // 0..4
}

#[derive(Debug, Clone, Copy)]
struct BorderAlignment {
  tile: usize,
  border: u16,
  flip: bool,
  rot: u8, // 0..4
}

impl std::fmt::Display for BorderAlignment {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "BorderAlignment {{")?;
    writeln!(f, "  tile: {}", self.tile)?;
    writeln!(f, "  border: {:010b}", self.border)?;
    writeln!(f, "  flip: {}", self.flip)?;
    writeln!(f, "  rot: {}", self.rot)?;
    writeln!(f, "}}")?;

    Ok(())
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let tiles: HashMap<usize, Tile> = lines
    .iter()
    .filter(|line| !line.is_empty())
    .chunks(11)
    .into_iter()
    .map(|mut chunk| {
      let tile_id: usize = chunk
        .next()
        .and_then(|line| line.strip_prefix("Tile "))
        .and_then(|line| line.strip_suffix(":"))
        .and_then(|line| line.parse().ok())
        .unwrap();

      let tile_data =
        chunk
          .enumerate()
          .fold(Array2::default((10, 10)), |acc, (y, line)| {
            line.chars().enumerate().fold(acc, |mut acc, (x, c)| {
              if c == '#' {
                acc[(x, y)] = true;
              }
              acc
            })
          });
      let tile = Tile { data: tile_data };

      (tile_id, tile)
    })
    .collect();

  // Store all borders as a TileAlignment, ensuring the border is at the TOP of the tile, and is the smallest possible border, bitwise
  let borders: HashMap<u16, Vec<TileAlignment>> = tiles
    .iter()
    .flat_map(|(tile_id, tile)| {
      (0..4).map(|bid| {
        let border = tile.get_border(bid);
        let flipped = flip_border(border);
        let flip = flipped < border;
        let alignment = TileAlignment {
          tile: *tile_id,
          flip,
          rot: bid,
        };

        (std::cmp::min(border, flipped), alignment)
      })
    })
    .fold(HashMap::new(), |mut acc, (key, val)| {
      acc.entry(key).or_default().push(val);
      acc
    });

  let uniques = borders
    .values()
    .filter_map(|alignments| {
      (alignments.len() == 1)
        .then(|| alignments.get(0))
        .flatten()
        .map(|alignment| alignment.tile)
    })
    .counts();

  let corners = uniques
    .iter()
    .filter_map(|(tile, count)| (*count == 2).then_some(tile))
    .collect_vec();

  let p1 = corners.iter().copied().product::<usize>();

  println!("Part 1: {}", p1);

  assert!(corners.len() == 4);

  // To construct the image:
  // Pick an arbitrary corner piece and rotate so that it fits the top left.
  // Create a queue of borders which need matching. Each position should only have one "key" border.
  // Flood-fill along the queue, dropping edges which can't be matched (exist on the edge)
  //
  // The hard part is applying a tile.
  // All borders are stored as a minimum of themselves (covering flips), saving whether they were flipped or not.
  // Thus, we should always search for a border by taking the minimum of the key, and storing whether we need to flip it or not.
  // `xor` the flips together to determine whether the tile needs to be flipped or not, apply this
  // Calculating rotation:
  // All borders are stored as the top, with some amount of rotation to get there from their original state.
  // Each key border will also store its rotation. Key borders will ALWAYS be either RIGHT (1) or BOTTOM (2).
  // To match up to the key border, the inverse rotation is needed, (ROT + 2) % 4, (so that the target is LEFT (3) or TOP (0)).
  // Let's say a sought border was originally it's tile's RIGHT border (1). It'll be stored in the search queue with ROT = 1.
  // Let's also say we're seeking it to match to a RIGHT key (1), with inverse rotation LEFT (3). Obviously, the original tile must
  // take a final ROT of 2 (so the RIGHT edge becomes the LEFT), which we can calculate with the difference: LEFT - ROT = 3 - 1 = 2
  // so we take (KEY_ROT + 2) % 4 - TILE_ROT. To avoid underflow, we may extend this to (KEY_ROT + 6 - TILE_ROT) % 4.
  // It's actually unneeded to do this, since KEY_ROT is guaranteed to be >= 1 and TILE_ROT <= 3, but it's better practise!
  // So inclusion, we store (KEY_ROT + 6 - TILE_ROT) % 4 as the needed rotation alongside the tile information, fetch the new borders,
  // and add them to the new queue.
  //
  // To fetch the new borders, we have to apply the accumulated transformation, which may include a FLIP (relative to TILE_ROT)
  // and then some ROT (calculated above).
  // If FLIP, we have to decide whether it's a FLIP_H or FLIP_V. If TILE_ROT is even, it was a TOP or BOTTOM piece, so we take FLIP_H, otherwise, FLIP_V.
  // After applying the FLIP to the original tile, we may then apply the calculated ROT. And, you're done!

  let mut tiles = tiles; // tiles needs to be mutable here so we can remove them
  let picture_size = tiles.len().isqrt();
  let mut picture: Array2<Option<Tile>> =
    Array2::default((picture_size, picture_size));
  let mut key_corner = tiles.remove(corners[0]).unwrap();

  fn debug_picture(picture: &Array2<Option<Tile>>) -> String {
    let size = ((picture.nrows()) * 11) - 1;
    let mut chrarray = Array2::from_elem((size, size), ' ');
    for ((x, y), tile) in picture.indexed_iter() {
      let Some(tile) = tile else { continue };
      let (tx, ty) = (x * 11, y * 11);
      for ((cx, cy), c) in tile.data.indexed_iter() {
        chrarray[(tx + cx, ty + cy)] = if *c { '#' } else { '.' };
      }
    }
    chrarray
      .lanes(Axis(0))
      .into_iter()
      .map(|lane| lane.iter().collect::<String>())
      .join("\n")
  }

  // now we have to find how we need to rotate the key corner piece
  let rightern_edge = (0..4)
    .map(|bid| (bid, key_corner.get_border(bid)))
    .filter_map(|(bid, border)| {
      (borders
        .get(&{
          let flipped = flip_border(border);
          std::cmp::min(flipped, border)
        })
        .unwrap()
        .len()
        == 1)
        .then_some(bid)
    })
    .reduce(|a, b| match (a, b) {
      (3, 0) => 0,
      (0, 3) => 0,
      _ => std::cmp::max(a, b),
    })
    .unwrap();

  key_corner.rotate(invert_rotation(rightern_edge));

  // map slots to borders/rotations
  let mut queue: HashMap<(usize, usize), BorderAlignment> = HashMap::new();

  queue.extend((1..=2).map(|bid| {
    let (x, y) = (bid % 2, (bid + 1) % 2);
    let border = key_corner.get_border(bid);
    let flipped = flip_border(border);
    let alignment = BorderAlignment {
      tile: *corners[0],
      border: std::cmp::min(flipped, border),
      flip: flipped < border,
      rot: bid,
    };

    ((x as usize, y as usize), alignment)
  }));

  picture[(0, 0)] = Some(key_corner);

  while !queue.is_empty() {
    for ((tx, ty), alignment) in std::mem::take(&mut queue).into_iter() {
      let Some(matching_alignment) = borders
        .get(&alignment.border)
        .and_then(|v| v.iter().find(|a| a.tile != alignment.tile))
      else {
        println!("## CRITICAL FAILURE ##");
        println!("failed on tile {}, {}", tx, ty);
        println!("{}", alignment);
        std::process::exit(1);
      };

      let flip = !alignment.flip != matching_alignment.flip;
      let rot = (alignment.rot + 6 - matching_alignment.rot) % 4;

      let mut tile = tiles.remove(&matching_alignment.tile).unwrap();

      if flip {
        if matching_alignment.rot % 2 == 0 {
          tile.flip_h();
        } else {
          tile.flip_v();
        }
      }

      tile.rotate(rot);

      queue.extend((1..=2).filter_map(|bid| {
        let (x, y) = (tx + (bid % 2) as usize, (ty + ((bid + 1) % 2) as usize));
        if x >= picture_size || y >= picture_size {
          return None;
        }
        let border = tile.get_border(bid);
        let flipped = flip_border(border);
        let alignment = BorderAlignment {
          tile: matching_alignment.tile,
          border: std::cmp::min(flipped, border),
          flip: flipped < border,
          rot: bid,
        };

        Some(((x as usize, y as usize), alignment))
      }));

      picture[(tx, ty)] = Some(tile);
    }
  }

  //println!("{}", debug_picture(&picture));

  // Ok, time to condense the image.
  let final_size = picture_size * 8;
  let mut final_view: Array2<bool> = Array2::default((final_size, final_size));

  for ((x, y), tile) in picture.indexed_iter() {
    let (tx, ty) = (x * 8, y * 8);
    for ((cx, cy), b) in tile
      .as_ref()
      .unwrap()
      .data
      .slice(s![1..=8, 1..=8])
      .indexed_iter()
    {
      final_view[(tx + cx, ty + cy)] = *b;
    }
  }

  let final_view_tile = Tile { data: final_view }; // this is a horrible shortcut and I hate it but oh well

  #[allow(non_snake_case)]
  let KERNEL = {
    let mut k = arr2(&[
      [
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, true, false,
      ],
      [
        true, false, false, false, false, true, true, false, false, false,
        false, true, true, false, false, false, false, true, true, true,
      ],
      [
        false, true, false, false, true, false, false, true, false, false,
        true, false, false, true, false, false, true, false, false, false,
      ],
    ]);
    k.swap_axes(0, 1); // since arrays are instantiated with axis 1 = x, and we do it backwards
    k.map_inplace(|b| *b = !*b); // operand is !k + p, so we preinvert k
    k
  };

  // ok we have to try all possible rotations, plus flipped horizontally and vertically (separately, but not together)

  let [k_width, k_height] = KERNEL.shape() else {
    unreachable!()
  };
  let x_bound = final_size - k_width;
  let y_bound = final_size - k_height;

  let serpents = (0..4)
    .cartesian_product(0..3)
    .find_map(|(rot, flipping)| {
      let mut current_view = final_view_tile.clone();
      match flipping {
        0 => {}
        1 => current_view.flip_h(),
        2 => current_view.flip_v(),
        _ => unreachable!(),
      }
      current_view.rotate(rot);

      let serpents = (0..x_bound)
        .cartesian_product(0..y_bound)
        .filter(|(cx, cy)| {
          current_view
            .data
            .slice(s![*cx..*cx + k_width, *cy..*cy + k_height])
            .iter()
            .zip(KERNEL.iter())
            .all(|(p, k)| *p || *k)
        })
        .count();

      (serpents > 0).then_some(serpents)
    })
    .unwrap();

  // assuming serpents are non-overlapping, that is, they don't share any hashes:
  let num_hash_per_serpent = KERNEL.iter().filter(|b| !*b).count();
  let num_hashes_total = final_view_tile.data.iter().filter(|b| **b).count();

  println!(
    "Part 2: {}",
    num_hashes_total - num_hash_per_serpent * serpents
  );
}
