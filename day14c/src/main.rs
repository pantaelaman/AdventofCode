use grid::Grid;
use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, p2) = puzzle(&mut reader);
    println!("Part 1: {}\nPart 2: {}", p1, p2);
}

fn puzzle<F: Read>(reader: &mut BufReader<F>) -> (usize, usize) {
    let literal_grid = reader
        .lines()
        .map(|line| {
            let line = line.unwrap();
            line.chars().collect_vec()
        })
        .collect_vec();
    let ncols = literal_grid[0].len();
    let mut landscape = Grid::from_vec(
        literal_grid.iter().flatten().map(|c| *c).collect_vec(),
        ncols,
    );

    let first_weight = {
        let mut internal = landscape.clone();
        tilt_grid_north(&mut internal);
        calculate_weights(&internal)
    };

    let mut visited: Vec<Grid<char>> = vec![landscape.clone()];
    let last_weight = loop {
        cycle_grid(&mut landscape);
        if let Some(i) = visited.iter().position(|g| g == &landscape) {
            let cycle_length = visited.len() - i;
            let last = i + ((1_000_000_000 - i) % cycle_length);
            break calculate_weights(&visited[last]);
        }
        visited.push(landscape.clone());
    };

    (first_weight, last_weight)
}

fn tilt_grid_north(grid: &mut Grid<char>) {
    for col in 0..grid.cols() {
        let mut anchor = 0;
        for row in 0..grid.rows() {
            let cur = *grid.get(row, col).unwrap();
            match cur {
                '#' => anchor = row + 1,
                'O' => {
                    let swap = std::mem::replace(grid.get_mut(anchor, col).unwrap(), cur);
                    *grid.get_mut(row, col).unwrap() = swap;
                    anchor += 1;
                }
                '.' => {}
                _ => unreachable!(),
            }
        }
    }
}

fn cycle_grid(grid: &mut Grid<char>) {
    for _ in 0..4 {
        tilt_grid_north(grid);
        grid.rotate_right();
    }
}

fn calculate_weights(grid: &Grid<char>) -> usize {
    let nrows = grid.rows();
    grid.iter_rows()
        .enumerate()
        .map(|(i, row)| row.filter(|c| **c == 'O').count() * (nrows - i))
        .sum()
}
