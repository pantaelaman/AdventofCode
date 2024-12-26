#![feature(impl_trait_in_assoc_type)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]
#![feature(step_trait)]
#![feature(const_swap)]
use std::{
  cmp::Ordering,
  collections::HashSet,
  hash::Hash,
  iter::Step,
  ops::{Add, Mul, Sub},
};

use itertools::Itertools;
use num_traits::{CheckedAdd, CheckedSub, One, PrimInt, Zero};

/// A glorified tuple type containing two of the same type of element,
/// usually numbers.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Point<N> {
  pub x: N,
  pub y: N,
}

pub trait AbsDiff<Rhs = Self> {
  type Output;

  fn abs_diff(self, rhs: Rhs) -> Self::Output;
}

impl<N: PrimInt> AbsDiff for N {
  type Output = N;

  fn abs_diff(self, rhs: Self) -> Self::Output {
    std::cmp::max(self, rhs) - std::cmp::min(self, rhs)
  }
}

impl<N: AbsDiff<Output = N>> AbsDiff for Point<N> {
  type Output = Point<N>;

  /// Calculates the absolute difference between the x and y values.
  fn abs_diff(self, rhs: Self) -> Self::Output {
    Point {
      x: self.x.abs_diff(rhs.x),
      y: self.y.abs_diff(rhs.y),
    }
  }
}

impl<N: AbsDiff<Output = N> + Add<Output = N>> Point<N> {
  /// Calculates the manhattan or taxicab distance between two points.
  pub fn manhattan(self, other: Self) -> N {
    let diff = self.abs_diff(other);
    diff.x + diff.y
  }
}

/// Implemented primarily for use with [`.contains()`](std::ops::Range::contains).
/// Points whose x and y disagree in ordering form no particular order.
impl<N: PartialOrd> PartialOrd for Point<N> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    match (self.x.partial_cmp(&other.x)?, self.y.partial_cmp(&other.y)?) {
      (Ordering::Less, Ordering::Less) => Some(Ordering::Less),
      (Ordering::Greater, Ordering::Greater) => Some(Ordering::Greater),
      (Ordering::Equal, Ordering::Equal) => Some(Ordering::Equal),
      _ => None,
    }
  }
}

impl<N: Step> Point<N> {
  /// Point cannot implement [`Step`] on its own, and so
  /// [`Range`](std::ops::Range) cannot implement [`Iterator`] for it. This method
  /// allows for similar functionality.
  pub fn all_between(self, other: Self) -> impl Iterator<Item = Point<N>> {
    (self.x..other.x)
      .cartesian_product(self.y..other.y)
      .map(Into::into)
  }
}

impl<N> Point<N> {
  /// Shorthand for literal construction.
  pub const fn new(x: N, y: N) -> Self {
    Point { x, y }
  }

  /// Apply the same function to both members of the point.
  /// This can be used to convert between point types as in:
  /// ```
  /// let p1: Point<u32> = Point::new(1, 0);
  /// let p2: Point<i64> = p1.map(Into::into);
  /// ```
  pub fn map<O>(self, mut op: impl FnMut(N) -> O) -> Point<O> {
    Point {
      x: (op)(self.x),
      y: (op)(self.y),
    }
  }
}

impl<O, N: Mul<Output = O> + Copy> Mul<N> for Point<N> {
  type Output = Point<O>;

  /// Scalar multiplication. Backwards of standard mathematical notation,
  /// but implementing `Mul<Point<N>>` for `N` is disallowed by the compiler.
  fn mul(self, rhs: N) -> Self::Output {
    Point {
      x: self.x * rhs,
      y: self.y * rhs,
    }
  }
}

impl<O, N: Mul<Output = O> + Copy> Point<N> {
  /// Multiply both `x` and `y` by a scalar value of their type.
  pub fn scale(self, scalar: N) -> Point<O> {
    Point {
      x: self.x * scalar,
      y: self.y * scalar,
    }
  }
}

impl<N: Add<Output = N> + Sub<Output = N> + One + Copy> Point<N> {
  /// Get the neighbours in the cardinal directions.
  /// This method uses the unchecked versions of subtraction and addition;
  /// for bounds checking, use [`checked_orthogonal_neighbours`](Self::checked_orthogonal_neighbours).
  pub fn orthogonal_neighbours(self) -> impl Iterator<Item = Point<N>> {
    [
      (self.x + N::one(), self.y).into(),
      (self.x - N::one(), self.y).into(),
      (self.x, self.y + N::one()).into(),
      (self.x, self.y - N::one()).into(),
    ]
    .into_iter()
  }

  pub fn all_neighbours(self) -> impl Iterator<Item = Point<N>> {
    self.orthogonal_neighbours().chain([
      (self.x + N::one(), self.y + N::one()).into(),
      (self.x - N::one(), self.y + N::one()).into(),
      (self.x + N::one(), self.y - N::one()).into(),
      (self.x - N::one(), self.y - N::one()).into(),
    ])
  }
}

impl<N: CheckedAdd<Output = N> + CheckedSub<Output = N> + One + Copy> Point<N> {
  /// Get the neighbours in the cardinal directions, excluding ones
  /// outside the bounds of `N`. For the unchecked version, see
  /// [`orthogonal_neighbours`](Self::orthogonal_neighbours).
  pub fn checked_orthogonal_neighbours(self) -> impl Iterator<Item = Point<N>> {
    [
      self.x.checked_add(&N::one()).map(|x| Point::new(x, self.y)),
      self.x.checked_sub(&N::one()).map(|x| Point::new(x, self.y)),
      self.y.checked_add(&N::one()).map(|y| Point::new(self.x, y)),
      self.y.checked_sub(&N::one()).map(|y| Point::new(self.x, y)),
    ]
    .into_iter()
    .filter_map(|p| p)
  }
}

impl<N: Ord + Zero> Point<N> {
  /// Effectively `(0..max_x).contains(x) && (0..max_y).contains(y)` where
  /// `0` is [`Zero`].
  #[inline]
  pub fn within_bounds(&self, max_x: N, max_y: N) -> bool {
    self.x < max_x
      && self.y < max_y
      && self.x >= N::zero()
      && self.y >= N::zero()
  }

  /// Convenience function, like [`within_bounds`](Self::within_bounds) but takes a point.
  #[inline]
  pub fn within_point(&self, max: Point<N>) -> bool {
    self.within_bounds(max.x, max.y)
  }
}

impl<N: Ord> Point<N> {
  /// Effectively `(min_x..max_x).contains(x) && (min_y..max_y).contains(y)`.
  #[inline]
  pub fn between_bounds(&self, min_x: N, min_y: N, max_x: N, max_y: N) -> bool {
    self.x >= min_x && self.y >= min_y && self.x < max_x && self.y < max_y
  }

  /// Convenience function, like [`between_bounds`](Self::between_bounds) but takes points.
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

pub const fn gcd(mut a: usize, mut b: usize) -> usize {
  if a == 0 {
    return b;
  } else if b == 0 {
    return a;
  }

  let i = a.trailing_zeros();
  let j = a.trailing_zeros();
  let k = if i > j { j } else { i }; // min isn't const ig

  a = a >> i;
  b = b >> j;

  loop {
    if a > b {
      std::mem::swap(&mut a, &mut b);
    }

    b -= a;

    if b == 0 {
      return a << k;
    }

    b >>= b.trailing_zeros();
  }
}

/// Compile-time based modular number.
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

impl<const B1: usize> Mod<B1> {
  /// Chinese Remainder Theorem; in taking two numbers `n1 mod B1` and `n2 mod B2`
  /// in which `n1 == n2`, `n1` and conversely `n2` can be solved for as long as
  /// `B1` and `B2` are coprime; i.e. `gcd(B1, B2) == 1`. This is checked by the
  /// trait bounds for this function.
  pub fn crt<const B2: usize>(self, other: Mod<B2>) -> usize
  where
    Assert<{ gcd(B1, B2) == 1 }>: IsTrue,
  {
    for i in 0.. {
      let num = self.value + (i * B1);
      if num % B2 == other.value {
        return num;
      }
    }
    unreachable!()
  }
}

/// Takes a list of strings and splits it into an iterator of chars and their coordinates,
/// relative to the first char in the first string.
pub fn grid_input<'a, I, N>(
  input: I,
) -> impl Iterator<Item = (Point<N>, char)> + use<'a, I, N>
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

/// Holds some check over const generics.
pub enum Assert<const BOUND: bool> {}
/// Implemented for `Assert<true>`.
pub trait IsTrue {}
impl IsTrue for Assert<true> {}
