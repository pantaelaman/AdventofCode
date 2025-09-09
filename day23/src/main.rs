use std::{
  io::stdin,
  iter::successors,
  marker::PhantomData,
  ops::{Deref, DerefMut},
  pin::Pin,
};

use itertools::{iterate, Itertools};

struct CircularNode<T> {
  data: T,
  next: *mut CircularNode<T>,
}

struct Cursor<T> {
  len: usize,
  node: *mut CircularNode<T>,
}

struct OrphanSlice<T> {
  len: usize,
  first: *mut CircularNode<T>,
  last: *mut CircularNode<T>,
}

impl<T> OrphanSlice<T> {
  fn iter<'o>(&'o self) -> OrphanSliceIter<'o, T> {
    OrphanSliceIter {
      orphan_slice: self,
      position: 0,
      current: self.first,
    }
  }
}

struct OrphanSliceIter<'o, T> {
  orphan_slice: &'o OrphanSlice<T>,
  position: usize,
  current: *mut CircularNode<T>,
}

impl<'o, T> Iterator for OrphanSliceIter<'o, T> {
  type Item = &'o T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.position >= self.orphan_slice.len {
      return None;
    }

    self.position += 1;
    let res = unsafe { &(*self.current).data };

    unsafe {
      self.current = (*self.current).next;
    }

    Some(res)
  }
}

struct Subcursor<T> {
  inactive_cursor: Cursor<T>,
  active_cursor: Cursor<T>,
}

impl<T> Subcursor<T> {
  fn next(&mut self) {
    unsafe {
      self.active_cursor.node = (*self.active_cursor.node).next;
    }
  }

  fn get(&self) -> &T {
    unsafe { &(*self.active_cursor.node).data }
  }

  fn ascend(self) -> Cursor<T> {
    self.inactive_cursor
  }

  fn insert_slice(&mut self, slice: OrphanSlice<T>) {
    let OrphanSlice {
      len: slice_len,
      first,
      last,
    } = slice;

    unsafe {
      (*last).next = (*self.active_cursor.node).next;
      (*self.active_cursor.node).next = first;
    }

    self.inactive_cursor.len += slice_len;
    self.active_cursor.len += slice_len;
  }
}

impl<T> Cursor<T> {
  fn make_circle<I: IntoIterator<Item = T>>(it: I) -> Option<Self> {
    let mut it = it.into_iter();
    let first = Box::into_raw(Box::new({
      let data = it.next()?;
      CircularNode {
        data,
        next: std::ptr::null_mut(),
      }
    }));

    unsafe {
      (*first).next = first;
    }

    let mut last = first;

    let mut len = 1;

    for data in it {
      let new_last =
        Box::into_raw(Box::new(CircularNode { data, next: first }));
      unsafe {
        (*last).next = new_last;
      }
      last = new_last;
      len += 1;
    }

    Some(Cursor { len, node: first })
  }

  fn iter(&mut self) -> CursorIter<T> {
    CursorIter { cursor: self }
  }

  fn subcursor(self) -> Subcursor<T> {
    Subcursor {
      active_cursor: Cursor { ..self },
      inactive_cursor: self,
    }
  }

  fn next(&mut self) {
    unsafe {
      self.node = (*self.node).next;
    }
  }

  fn get<'d>(&'d self) -> &'d T {
    unsafe { &(*self.node).data }
  }

  fn orphan_slice(&mut self, slice_len: usize) -> OrphanSlice<T> {
    assert!(self.len > slice_len);

    let prefirst = self.node;

    self.next();
    let first = self.node;
    for _ in 0..slice_len - 1 {
      self.next();
    }
    let last = self.node;
    self.next();

    unsafe {
      (*prefirst).next = self.node;
    }

    self.len -= slice_len;

    OrphanSlice {
      first,
      last,
      len: slice_len,
    }
  }

  fn insert_slice(&mut self, slice: OrphanSlice<T>) {
    let OrphanSlice {
      len: slice_len,
      first,
      last,
    } = slice;

    unsafe {
      (*last).next = (*self.node).next;
      (*self.node).next = first;
    }

    self.len += slice_len;
  }
}

struct CursorIter<'d, T> {
  cursor: &'d mut Cursor<T>,
}

impl<'d, T> CursorIter<'d, T> {
  fn once(self) -> impl Iterator<Item = &'d T> {
    let len = self.cursor.len;
    self.take(len)
  }
}

impl<'d, T> Iterator for CursorIter<'d, T> {
  type Item = &'d T;

  fn next(&mut self) -> Option<Self::Item> {
    let res = unsafe { &(*self.cursor.node).data };
    self.cursor.next();
    Some(res)
  }
}

fn print_list<T: std::fmt::Debug>(cursor: &mut Cursor<T>) {
  print!("[");
  for item in cursor.iter().once() {
    print!("{:?}, ", item);
  }
  println!("]");
}

fn print_subcursor<T: std::fmt::Debug>(cursor: &mut Subcursor<T>) {
  print!(" > {} [", cursor.active_cursor.len);
  for _ in 0..cursor.active_cursor.len {
    print!("{:?}, ", cursor.get());
    cursor.next();
  }
  println!("]");
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");
  let init = lines[0]
    .chars()
    .map(|c| c.to_digit(10).unwrap() as usize)
    .collect_vec();

  let mut cursor = Cursor::make_circle(init.iter().copied()).unwrap();

  print_list(&mut cursor);

  for _ in 0..100 {
    cursor = do_move(cursor, 9);
    print_list(&mut cursor);
  }

  print_list(&mut cursor);

  let mut it = cursor.iter();
  it.find(|d| **d == 1);
  let p1 = it
    .once()
    .map(|d| char::from_digit(*d as u32, 10).unwrap())
    .collect::<String>();
  println!("Part 1: {}", p1.strip_suffix('1').unwrap());
}

fn do_move(mut cursor: Cursor<usize>, val_max: usize) -> Cursor<usize> {
  let curcup = *cursor.get();

  let chunk = cursor.orphan_slice(3);

  //print!("[");
  //for n in chunk.iter().take(chunk.len) {
  //  print!("{:?}, ", n);
  //}
  //println!("]");

  let destcup = successors(Some(curcup), |c| {
    Some(match c {
      1 => val_max,
      c => c - 1,
    })
  })
  .skip(1)
  .find(|v| chunk.iter().find(|e| *e == v).is_none())
  .unwrap();

  let mut subcursor = cursor.subcursor();

  while *subcursor.get() != destcup {
    subcursor.next();
  }

  subcursor.insert_slice(chunk);

  subcursor.ascend()
}
