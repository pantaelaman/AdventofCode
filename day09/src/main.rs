#![feature(linked_list_remove)]
use std::{
  collections::{HashSet, VecDeque},
  fs::File,
  io::Read,
};

use itertools::repeat_n;

#[derive(Debug, Clone, Copy)]
struct Chunk {
  ty: ChunkType,
  len: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ChunkType {
  File(usize),
  Free,
}

impl ChunkType {
  fn get_file(&self) -> usize {
    match self {
      ChunkType::File(file_id) => *file_id,
      _ => unimplemented!(),
    }
  }
}

fn main() {
  let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let mut buf = String::new();
  file.read_to_string(&mut buf).unwrap();

  let starting_chunks: VecDeque<Chunk> = buf
    .trim()
    .chars()
    .map(|c| c.to_digit(10).unwrap())
    .enumerate()
    .map(|(i, len)| {
      let ty = if i % 2 == 0 {
        ChunkType::File(i / 2)
      } else {
        ChunkType::Free
      };
      Chunk {
        ty,
        len: len as usize,
      }
    })
    .collect();

  let mut chunks = starting_chunks.clone();

  loop {
    let mut next = chunks.pop_back().unwrap();
    if let ChunkType::File(file_id) = next.ty {
      let (idx, free_chunk) = match chunks
        .iter_mut()
        .enumerate()
        .find(|(_, el)| el.ty == ChunkType::Free)
      {
        Some(v) => v,
        None => {
          chunks.push_back(next);
          break;
        }
      };
      if free_chunk.len <= next.len {
        let free_chunk = *free_chunk;
        chunks.remove(idx);
        chunks.insert(
          idx,
          Chunk {
            ty: ChunkType::File(file_id),
            len: free_chunk.len,
          },
        );
        next.len -= free_chunk.len;
        if next.len != 0 {
          chunks.push_back(next);
        }
      } else {
        free_chunk.len -= next.len;
        chunks.insert(
          idx,
          Chunk {
            ty: ChunkType::File(file_id),
            len: next.len,
          },
        )
      }
    }
  }

  let checksum = chunks
    .iter()
    .flat_map(|chunk| repeat_n(chunk.ty.get_file(), chunk.len))
    .enumerate()
    .map(|(a, b)| a * b)
    .sum::<usize>();

  println!("Part 1: {checksum}");

  let mut chunks = starting_chunks.clone();

  let mut seen_files: HashSet<usize> = HashSet::new();
  loop {
    let (idx, next) =
      match chunks
        .iter()
        .enumerate()
        .rfind(|(_, chunk)| match chunk.ty {
          ChunkType::File(id) => !seen_files.contains(&id),
          _ => false,
        }) {
        Some(v) => v,
        None => break,
      };

    seen_files.insert(next.ty.get_file());

    let free_chunk_idx = match chunks
      .iter()
      .enumerate()
      .take_while(|(i, _)| i < &idx)
      .position(|(_, chunk)| {
        chunk.ty == ChunkType::Free && chunk.len >= next.len
      }) {
      Some(v) => v,
      None => continue,
    };

    let next = chunks.remove(idx).unwrap();
    chunks.insert(
      idx,
      Chunk {
        ty: ChunkType::Free,
        len: next.len,
      },
    );

    let mut free_chunk = chunks.remove(free_chunk_idx).unwrap();

    free_chunk.len -= next.len;
    if free_chunk.len != 0 {
      chunks.insert(free_chunk_idx, free_chunk);
    }

    chunks.insert(free_chunk_idx, next);

    merge_free(&mut chunks);
  }

  let checksum = chunks
    .iter()
    .flat_map(|chunk| {
      repeat_n(
        match chunk.ty {
          ChunkType::File(id) => id,
          _ => 0,
        },
        chunk.len,
      )
    })
    .enumerate()
    .map(|(a, b)| a * b)
    .sum::<usize>();

  println!("Part 2: {}", checksum);
}

fn merge_free(chunks: &mut VecDeque<Chunk>) {
  let mut i = 1;
  while i < chunks.len() {
    if chunks[i - 1].ty == ChunkType::Free && chunks[i].ty == ChunkType::Free {
      chunks[i - 1].len += chunks[i].len;
      chunks.remove(i);
    } else {
      i += 1;
    }
  }
}
