use std::fs::File;
use std::io::{BufRead,BufReader};

fn main() {
    process_input("Day11Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let mut map: [[(i8,bool); 10]; 10] = [[(-1, true); 10]; 10];
    let reader = BufReader::new(file);

    for (row,line) in reader.lines().take(10).enumerate() {
        let line = line.expect(format!("Could not read line {}", row).as_str());
        for (col,octo) in line.chars().take(10).enumerate() {
            let octo = octo.to_string().parse::<i8>().expect("Could not parse");
            map[col][row] = (octo, false);
        }
    }

    let mut until = false;
    let mut step = 0;
    while !until {
        step += 1;
        if run_step(&mut map) == 100 {
            until = true;
        }
        println!("Finished step {}", step);
    }
    println!("Final step: {}", step);
}

fn run_step(map: &mut [[(i8,bool); 10]; 10]) -> i32 {
    let mut result = 0;
    for x in 0..10 {
        for y in 0..10 {
            flash_point((x,y), map, &mut result);
        }
    }
    for x in 0..10 {
        for y in 0..10 {
            if map[x][y].1 {
                map[x][y].0 = 0;
                map[x][y].1 = false;
            }
        }
    }
    result
}

fn flash_point(pos: (usize,usize), map: &mut [[(i8,bool); 10]; 10], count: &mut i32) {
    let point = &mut map[pos.0][pos.1];
    point.0 += 1;
    if point.0 >= 10 {
        if !point.1 {
            point.1 = true;
            *count += 1;
            for i in [-1_i8,0_i8,1_i8] {
                for j in [-1_i8,0_i8,1_i8] {
                    let coord = (pos.0 as i8 + i,pos.1 as i8 + j);
                    if (coord.0 as usize, coord.1 as usize) != pos && !(coord.0 >= 10 || coord.1 >= 10 || coord.0 < 0 || coord.1 < 0) {
                        flash_point((coord.0 as usize, coord.1 as usize), map, count);
                    }
                }
            }
        }
    }
}
