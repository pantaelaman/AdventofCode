use std::{
  collections::HashMap,
  hash::Hash,
  ops::{Add, Mul, Sub},
};

use num_traits::{CheckedAdd, CheckedSub, One, Zero};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Point<N> {
  pub x: N,
  pub y: N,
}

impl<N> Point<N> {
  pub fn new(x: N, y: N) -> Self {
    Point { x, y }
  }
}

impl<O, N: Mul<Output = O> + Copy> Point<N> {
  pub fn scale(self, scalar: N) -> Point<O> {
    Point {
      x: self.x * scalar,
      y: self.y * scalar,
    }
  }
}

impl<N: Add<Output = N> + Sub<Output = N> + One + Copy> Point<N> {
  pub fn orthogonal_neighbours(self) -> impl Iterator<Item = Point<N>> {
    [
      (self.x + N::one(), self.y).into(),
      (self.x - N::one(), self.y).into(),
      (self.x, self.y + N::one()).into(),
      (self.x, self.y - N::one()).into(),
    ]
    .into_iter()
  }
}

impl<N: Ord + Zero> Point<N> {
  #[inline]
  pub fn within_bounds(&self, max_x: N, max_y: N) -> bool {
    self.x < max_x
      && self.y < max_y
      && self.x >= N::zero()
      && self.y >= N::zero()
  }

  #[inline]
  pub fn within_point(&self, max: Point<N>) -> bool {
    self.within_bounds(max.x, max.y)
  }
}

impl<N: Ord> Point<N> {
  #[inline]
  pub fn between_bounds(&self, min_x: N, min_y: N, max_x: N, max_y: N) -> bool {
    self.x >= min_x && self.y >= min_y && self.x < max_x && self.y < max_y
  }

  #[inline]
  pub fn between_points(&self, min: Self, max: Self) -> bool {
    self.between_bounds(min.x, min.y, max.x, max.y)
  }
}

impl<O, N: Add<Output = O>> Add for Point<N> {
  type Output = Point<O>;

  fn add(self, rhs: Self) -> Self::Output {
    Point {
      x: self.x + rhs.x,
      y: self.y + rhs.y,
    }
  }
}

impl<O, N: Sub<Output = O>> Sub for Point<N> {
  type Output = Point<O>;

  fn sub(self, rhs: Self) -> Self::Output {
    Point {
      x: self.x - rhs.x,
      y: self.y - rhs.y,
    }
  }
}

impl<N: CheckedAdd> CheckedAdd for Point<N> {
  fn checked_add(&self, rhs: &Self) -> Option<Self> {
    self
      .x
      .checked_add(&rhs.x)
      .and_then(|x| self.y.checked_add(&rhs.y).map(|y| (x, y).into()))
  }
}

impl<N: CheckedSub> CheckedSub for Point<N> {
  fn checked_sub(&self, rhs: &Self) -> Option<Self> {
    self
      .x
      .checked_sub(&rhs.x)
      .and_then(|x| self.y.checked_sub(&rhs.y).map(|y| (x, y).into()))
  }
}

impl<N> From<(N, N)> for Point<N> {
  fn from(value: (N, N)) -> Self {
    Point {
      x: value.0,
      y: value.1,
    }
  }
}

impl<N> Into<(N, N)> for Point<N> {
  fn into(self) -> (N, N) {
    (self.x, self.y)
  }
}

pub struct Mod<const BASE: usize> {
  pub value: usize,
}

impl<const BASE: usize> std::ops::Deref for Mod<BASE> {
  type Target = usize;

  fn deref(&self) -> &Self::Target {
    &self.value
  }
}

impl<const BASE: usize> From<usize> for Mod<BASE> {
  fn from(value: usize) -> Self {
    Self { value }
  }
}

impl<const BASE: usize> Add for Mod<BASE> {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    ((self.value + rhs.value) % BASE).into()
  }
}

impl<const BASE: usize> Mul for Mod<BASE> {
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output {
    ((self.value * rhs.value) % BASE).into()
  }
}

impl<const BASE: usize> Sub for Mod<BASE> {
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output {
    let (val, overflowed) = self.value.overflowing_sub(rhs.value);
    if overflowed {
      (BASE - (usize::MAX - val)).into()
    } else {
      val.into()
    }
  }
}

pub fn grid_input<'a, I, O, N>(
  input: I,
) -> impl Iterator<Item = (Point<N>, char)> + use<'a, I, O, N>
where
  I: IntoIterator<Item = &'a str>,
  N: From<usize>,
{
  input.into_iter().enumerate().flat_map(|(y, line)| {
    line
      .chars()
      .enumerate()
      .map(move |(x, c)| ((x.into(), y.into()).into(), c))
  })
}

pub fn memoize<F, I, O>(op: F) -> impl FnMut(I) -> O + use<F, I, O>
where
  F: Fn(I) -> O,
  I: std::hash::Hash + Eq + Clone,
  O: Clone,
{
  let mut cache: HashMap<I, O> = HashMap::new();
  move |inp: I| -> O {
    if let Some(v) = cache.get(&inp) {
      return v.clone();
    }

    let v = op(inp.clone());
    cache.insert(inp, v.clone());
    v
  }
}
