use grid::Grid;
use itertools::Itertools;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read + Seek>(reader: &mut BufReader<F>) -> (usize, usize) {
    let lines = reader.lines();
    let height = lines.count();
    reader.rewind().unwrap();
    let grid = Grid::from_vec(
        reader
            .lines()
            .map(|line| line.unwrap().chars().collect_vec())
            .flatten()
            .collect_vec(),
        height,
    );
    let mut expanded_rows = HashSet::new();
    for (i, mut row) in grid.iter_rows().enumerate() {
        if row.all(|c| *c == '.') {
            expanded_rows.insert(i);
        }
    }
    let mut expanded_cols = HashSet::new();
    for (i, mut col) in grid.iter_cols().enumerate() {
        if col.all(|c| *c == '.') {
            expanded_cols.insert(i);
        }
    }
    grid.indexed_iter()
        .filter_map(|(i, c)| (*c == '#').then_some(i))
        .combinations(2)
        .fold((0, 0), |acc, gxs| {
            let (gx1, gx2) = (gxs[0], gxs[1]);
            // println!("{:?} <-> {:?}", gx1, gx2);
            let x_range = gx1.0.min(gx2.0)..gx1.0.max(gx2.0);
            let y_range = gx1.1.min(gx2.1)..gx1.1.max(gx2.1);
            let col_expansions = expanded_rows
                .iter()
                .filter(|n| x_range.contains(n))
                // .inspect(|n| println!("{n}"))
                .count();
            // println!("");
            let row_expansions = expanded_cols
                .iter()
                .filter(|n| y_range.contains(n))
                // .inspect(|n| println!("{n}"))
                .count();
            let diff = gx1.0.abs_diff(gx2.0) + gx1.1.abs_diff(gx2.1);
            // println!("\n{col_expansions} + {row_expansions} +: {diff}\n");
            (
                acc.0 + diff + col_expansions + row_expansions,
                acc.1 + diff + (col_expansions + row_expansions) * 999_999,
            )
        })
}
