use std::fs::File;
use std::io::{BufReader, Read, Seek};

fn main() {
    let mut file = File::open("input.txt").unwrap();
    println!("Part 1: {}", part1(&mut file).unwrap());
    println!("Part 2: {}", part2(&mut file).unwrap());
}

fn part2(input: &mut File) -> Option<usize> {
    [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .into_iter()
        .map(|slope| calc_slope(input, slope))
        .try_fold(1, |acc, trees| Some(acc * trees?))
}

fn part1(input: &mut File) -> Option<usize> {
    calc_slope(input, (3, 1))
}

fn calc_slope(input: &mut File, slope: (usize, usize)) -> Option<usize> {
    input.rewind().unwrap();
    let row_length = input
        .bytes()
        .position(|el| el.as_ref().map_or(false, |chr| chr == &b'\n'))?
        + 1;
    input.rewind().unwrap();
    let max_pos = input.metadata().unwrap().len() as usize;
    let mut reader = BufReader::new(input);
    let mut cur_pos: (usize, usize) = (0, 0);

    let mut count = 0;
    while xy_to_row(row_length, cur_pos.0, cur_pos.1) < max_pos {
        let mut buf = [0];
        reader.read(&mut buf).unwrap();
        if buf[0] == b'#' {
            count += 1;
        }

        // println!(
        //     "Buf {} at ({}, {}) [{}]",
        //     buf[0],
        //     cur_pos.0,
        //     cur_pos.1,
        //     xy_to_row(row_length, cur_pos.0, cur_pos.1)
        // );

        cur_pos.0 = (cur_pos.0 + slope.0) % (row_length - 1);
        cur_pos.1 += slope.1;
        reader
            .seek(std::io::SeekFrom::Start(
                xy_to_row(row_length, cur_pos.0, cur_pos.1) as u64,
            ))
            .unwrap();
    }

    Some(count)
}

fn xy_to_row(row_length: usize, x: usize, y: usize) -> usize {
    (row_length * y) + x
}
