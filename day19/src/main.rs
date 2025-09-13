use std::{
  collections::{HashMap, HashSet},
  io::stdin,
  iter::successors,
};

use itertools::{repeat_n, Itertools};
use nalgebra::{matrix, Matrix3, Matrix4, Rotation3, Vector3, Vector4};

const SHARED: usize = 6;
const THRESHOLD: usize = SHARED * (SHARED - 1) / 2;

const ROTATION_X: Matrix4<i32> = matrix![
    1, 0, 0, 0;
    0, 0, 1, 0;
    0, -1, 0, 0;
    0, 0, 0, 1];
const ROTATION_Y: Matrix4<i32> = matrix![
    0, 0, -1, 0;
    0, 1, 0, 0;
    1, 0, 0, 0;
    0, 0, 0, 1];
const ROTATION_Z: Matrix4<i32> = matrix![
    0, 1, 0, 0;
    -1, 0, 0, 0;
    0, 0, 1, 0;
    0, 0, 0, 1];

macro_rules! translation {
  ($x:expr, $y:expr, $z:expr) => {
    matrix![1, 0, 0, $x; 0, 1, 0, $y; 0, 0, 1, $z; 0, 0, 0, 1]
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let scanners = lines
    .iter()
    .batching(|lines| {
      lines.next()?;
      let mut beacons: HashSet<Vector4<i32>> = HashSet::new();
      while let Some(line) = lines.next() {
        if line.is_empty() {
          break;
        }

        let (x, y, z) = line
          .split(',')
          .map(|s| s.parse().unwrap())
          .collect_tuple()
          .unwrap();
        beacons.insert(Vector4::new(x, y, z, 1));
      }
      Some(beacons)
    })
    .map(|beacons| {
      let signature = beacons
        .iter()
        .tuple_combinations()
        .map(|(left, right)| {
          let diff = right - left;
          let dist_sq = diff.x.pow(2) + diff.y.pow(2) + diff.z.pow(2);
          dist_sq
        })
        .counts();

      (beacons, signature)
    })
    .collect_vec();

  let rotations: [Matrix4<i32>; 24] = [
    Matrix4::identity(),
    ROTATION_Y,
    ROTATION_Y * ROTATION_Y,
    ROTATION_Y * ROTATION_Y * ROTATION_Y,
    ROTATION_Z,
    ROTATION_Z * ROTATION_Z * ROTATION_Z,
    ROTATION_X,
    ROTATION_Y * ROTATION_X,
    ROTATION_Y * ROTATION_Y * ROTATION_X,
    ROTATION_Y * ROTATION_Y * ROTATION_Y * ROTATION_X,
    ROTATION_Z * ROTATION_X,
    ROTATION_Z * ROTATION_Z * ROTATION_Z * ROTATION_X,
    ROTATION_X * ROTATION_X,
    ROTATION_Y * ROTATION_X * ROTATION_X,
    ROTATION_Y * ROTATION_Y * ROTATION_X * ROTATION_X,
    ROTATION_Y * ROTATION_Y * ROTATION_Y * ROTATION_X * ROTATION_X,
    ROTATION_Z * ROTATION_X * ROTATION_X,
    ROTATION_Z * ROTATION_Z * ROTATION_Z * ROTATION_X * ROTATION_X,
    ROTATION_X * ROTATION_X * ROTATION_X,
    ROTATION_Y * ROTATION_X * ROTATION_X * ROTATION_X,
    ROTATION_Y * ROTATION_Y * ROTATION_X * ROTATION_X * ROTATION_X,
    ROTATION_Y * ROTATION_Y * ROTATION_Y * ROTATION_X * ROTATION_X * ROTATION_X,
    ROTATION_Z * ROTATION_X * ROTATION_X * ROTATION_X,
    ROTATION_Z * ROTATION_Z * ROTATION_Z * ROTATION_X * ROTATION_X * ROTATION_X,
  ];

  let mut transforms: HashMap<usize, HashMap<usize, Matrix4<i32>>> = scanners
    .iter()
    .enumerate()
    .tuple_combinations()
    .filter(|((_, (_, lsig)), (_, (_, rsig)))| {
      lsig
        .iter()
        .filter_map(|(dist, lv)| rsig.get(dist).map(|rv| std::cmp::min(lv, rv)))
        .sum::<usize>()
        >= THRESHOLD
    })
    .inspect(|((li, _), (ri, _))| println!("{},{} made it", li, ri))
    .filter_map(|((li, (lbeacons, _)), (ri, (rbeacons, _)))| {
      lbeacons
        .iter()
        .cartesian_product(rbeacons.iter())
        .cartesian_product(rotations.iter())
        .find_map(|((lbeacon, rbeacon), rrot)| {
          let new_rbeacon = rrot * rbeacon;
          let trans = lbeacon - new_rbeacon;
          let transform = translation!(trans.x, trans.y, trans.z) * rrot;

          let twelve_matching = rbeacons
            .iter()
            .map(move |beacon| transform * beacon)
            .filter(|beacon| lbeacons.contains(beacon))
            .count()
            >= SHARED;

          twelve_matching.then_some(((li, ri), transform))
        })
    })
    .fold(HashMap::new(), |mut acc, ((li, ri), transform)| {
      acc.entry(li).or_default().insert(ri, transform);
      acc
    });

  let mut beacons: HashSet<Vector4<i32>> = HashSet::new();

  beacons.extend(scanners[0].0.iter());

  let mut head = vec![(0, Matrix4::identity())];
  while let Some((lscanner, accum_transform)) = head.pop() {
    let Some(trs) = transforms.remove(&lscanner) else {
      continue;
    };

    for (rscanner, next_transform) in trs {
      let transform = accum_transform * next_transform;
      println!("scanner {rscanner} (from {lscanner}) w/ {:?}", transform);
      beacons.extend(
        scanners[rscanner]
          .0
          .iter()
          .map(move |beacon| transform * beacon),
      );

      head.push((rscanner, transform));
    }
  }

  println!("{}", beacons.len());
}
