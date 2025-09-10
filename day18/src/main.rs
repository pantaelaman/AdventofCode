use std::{io::stdin, iter::Peekable, str::FromStr};

use itertools::{
  Either::{self, Left, Right},
  Itertools,
};

struct NumberPair {
  left: *mut NumberNode,
  right: *mut NumberNode,
}

impl Drop for NumberPair {
  fn drop(&mut self) {
    for node in [self.left, self.right] {
      unsafe { std::mem::drop(Box::from_raw(node)) }
    }
  }
}

type NumberValue = Either<u8, NumberPair>;

struct NumberNode {
  depth: usize,
  parent: Option<Either<*mut NumberNode, *mut NumberNode>>,
  value: NumberValue,
}

impl std::fmt::Display for NumberNode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self.value {
      Left(v) => write!(f, "{v}")?,
      Right(pair) => write!(f, "[{},{}]", unsafe { &*pair.left }, unsafe {
        &*pair.right
      })?,
    }

    Ok(())
  }
}

impl NumberNode {
  fn magnitude(&self) -> usize {
    match self.value.as_ref() {
      Left(v) => *v as usize,
      Right(pair) => {
        3 * unsafe { &*pair.left }.magnitude()
          + 2 * unsafe { &*pair.right }.magnitude()
      }
    }
  }

  fn leftmost(&mut self) -> &mut u8 {
    let mut current = self;
    loop {
      match current.value.as_mut() {
        Left(v) => return v,
        Right(pair) => current = unsafe { &mut (*pair.left) },
      }
    }
  }

  fn rightmost(&mut self) -> &mut u8 {
    let mut current = self;
    loop {
      match current.value.as_mut() {
        Left(v) => return v,
        Right(pair) => current = unsafe { &mut (*pair.right) },
      }
    }
  }

  fn optical_left(&mut self) -> Option<&mut u8> {
    let mut current = self.parent;
    while let Some(left_top) = current {
      match left_top {
        Left(parent) => current = unsafe { (*parent).parent },
        Right(parent) => {
          return unsafe { &mut *parent }.left().map(NumberNode::rightmost);
        }
      }
    }
    None
  }

  fn optical_right(&mut self) -> Option<&mut u8> {
    let mut current = self.parent;
    while let Some(left_top) = current {
      match left_top {
        Left(parent) => {
          return unsafe { &mut *parent }.right().map(NumberNode::leftmost);
        }
        Right(parent) => current = unsafe { (*parent).parent },
      }
    }
    None
  }

  fn left(&mut self) -> Option<&mut Self> {
    self
      .value
      .as_mut()
      .right()
      .map(|pair| unsafe { &mut *pair.left })
  }

  fn right(&mut self) -> Option<&mut Self> {
    self
      .value
      .as_mut()
      .right()
      .map(|pair| unsafe { &mut *pair.right })
  }

  unsafe fn make_node(
    depth: usize,
    parent: Option<Either<*mut NumberNode, *mut NumberNode>>,
    value: NumberValue,
  ) -> *mut Self {
    Box::into_raw(Box::new(Self {
      depth,
      parent,
      value,
    }))
  }
}

struct Number {
  head: *mut NumberNode,
}

impl FromStr for Number {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    fn parse_from_stream<I: Iterator<Item = char>>(
      stream: &mut Peekable<I>,
      depth: usize,
      parent: Either<*mut NumberNode, *mut NumberNode>,
    ) -> Result<*mut NumberNode, ()> {
      match stream.next() {
        Some('[') => {
          let current = unsafe { Box::into_raw(Box::new(core::mem::zeroed())) };

          let left = parse_from_stream(stream, depth + 1, Left(current))?;

          let Some(',') = stream.next() else {
            return Err(());
          };

          let right = parse_from_stream(stream, depth + 1, Right(current))?;

          let Some(']') = stream.next() else {
            return Err(());
          };

          unsafe {
            *current = NumberNode {
              depth,
              parent: Some(parent),
              value: Right(NumberPair { left, right }),
            };
          }

          Ok(current)
        }
        Some(c) => {
          let val = c.to_digit(10).ok_or(())? as u8;
          Ok(Box::into_raw(Box::new(NumberNode {
            depth,
            parent: Some(parent),
            value: Left(val),
          })))
        }
        None => Err(()),
      }
    }

    let mut stream = s.chars().peekable();
    let Some('[') = stream.next() else {
      return Err(());
    };

    let current = unsafe { Box::into_raw(Box::new(core::mem::zeroed())) };

    let left = parse_from_stream(&mut stream, 1, Left(current))?;

    let Some(',') = stream.next() else {
      return Err(());
    };

    let right = parse_from_stream(&mut stream, 1, Right(current))?;

    let Some(']') = stream.next() else {
      return Err(());
    };

    unsafe {
      *current = NumberNode {
        depth: 0,
        parent: None,
        value: Right(NumberPair { left, right }),
      };
    }

    Ok(Number { head: current })
  }
}

impl Number {
  fn reduce(&mut self) {
    fn explode(num: &mut Number) -> bool {
      let mut queue = vec![num.head];
      while let Some(top) = queue.pop() {
        let top = unsafe { &mut (*top) };

        let Right(pair) = &top.value else {
          continue;
        };

        if top.depth < 4 {
          queue.extend([pair.right, pair.left]);
          continue;
        }

        let Left(left) = (unsafe { &(*pair.left).value }) else {
          unreachable!()
        };
        let Left(right) = (unsafe { &(*pair.right).value }) else {
          unreachable!()
        };

        if let Some(next_left) = top.optical_left() {
          *next_left += left;
        }
        if let Some(next_right) = top.optical_right() {
          *next_right += right;
        }

        core::mem::drop(core::mem::replace(&mut top.value, Left(0)));
        return true;
      }

      false
    }

    fn split(num: &mut Number) -> bool {
      let mut queue = vec![num.head];
      while let Some(top) = queue.pop() {
        let topv = unsafe { &mut (*top) };

        let v = match topv.value.as_ref() {
          Left(v) => {
            if *v >= 10 {
              *v
            } else {
              continue;
            }
          }
          Right(pair) => {
            queue.extend([pair.right, pair.left]);
            continue;
          }
        };

        let left = unsafe {
          NumberNode::make_node(topv.depth + 1, Some(Left(top)), Left(v / 2))
        };
        let right = unsafe {
          NumberNode::make_node(
            topv.depth + 1,
            Some(Right(top)),
            Left(v / 2 + v % 2),
          )
        };

        let pair = NumberPair { left, right };

        core::mem::drop(core::mem::replace(&mut topv.value, Right(pair)));
        return true;
      }

      false
    }

    loop {
      if !explode(self) {
        if !split(self) {
          break;
        }
      }
    }
  }

  unsafe fn parent(
    self,
    parent: Either<*mut NumberNode, *mut NumberNode>,
  ) -> *mut NumberNode {
    let mut queue = vec![self.head];
    while let Some(top) = queue.pop() {
      let top = unsafe { &mut *top };
      top.depth += 1;
      if let Some(pair) = top.value.as_ref().right() {
        queue.extend([pair.right, pair.left]);
      }
    }

    let head = unsafe { &mut *self.head };
    head.parent = Some(parent);
    head
  }

  fn magnitude(&self) -> usize {
    unsafe { &*self.head }.magnitude()
  }
}

impl std::ops::Add for Number {
  type Output = Number;

  fn add(self, rhs: Self) -> Self::Output {
    let top = unsafe { Box::into_raw(Box::new(core::mem::zeroed())) };

    unsafe {
      let left = self.parent(Left(top));
      let right = rhs.parent(Right(top));

      *top = NumberNode {
        depth: 0,
        parent: None,
        value: Right(NumberPair { left, right }),
      }
    }

    let mut num = Number { head: top };
    num.reduce();
    num
  }
}

impl std::fmt::Display for Number {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", unsafe { &*self.head })
  }
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let numbers = lines
    .iter()
    .map(|line| line.parse::<Number>().unwrap())
    .collect_vec();

  let sum = numbers.into_iter().reduce(std::ops::Add::add).unwrap();

  println!("Part 1: {}", sum.magnitude());

  let max_mag = lines
    .iter()
    .tuple_combinations()
    .map(|(left, right)| {
      (
        left.parse::<Number>().unwrap(),
        right.parse::<Number>().unwrap(),
      )
    })
    .map(|(left, right)| (left + right).magnitude())
    .max()
    .unwrap();

  println!("Part 2: {}", max_mag);
}
