use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::HashSet;

fn main() {
    println!("Test: {}", process_input("Day3test.txt"));
    println!("Result: {}", process_input("Day3input.txt"));
}

fn process_input(file: &str) -> usize {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);
    let input = reader.lines().next().expect("Empty file").expect("Could not read line 1");
    let mut houses: HashSet<(isize,isize)> = HashSet::new();
    let mut rposition: (isize,isize) = (0,0);
    let mut fposition: (isize,isize) = (0,0);
    let mut real_santa = true;

    houses.insert(rposition);

    for chr in input.chars() {
        let position = {
            if real_santa {&mut rposition} else {&mut fposition}
        };
        match chr {
            '^' => position.1+=1,
            'v' => position.1-=1,
            '<' => position.0-=1,
            '>' => position.0+=1,
            _ => panic!("Unexpected character"),
        }
        real_santa = !real_santa;
        houses.insert(*position);
    }

    houses.iter().count()
}
