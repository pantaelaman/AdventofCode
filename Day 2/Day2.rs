use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    process_input("Day2Input.txt");
    let data = [0,1,2,3,4,5,6];
    let mut data_iter = data.iter().cycle();
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);

    let orig_program = reader.lines().nth(0).unwrap().expect("Could not read line 0").split(",").map(|el| el.parse::<i32>().expect("Bad format in program")).collect::<Vec<i32>>();
    let mut program = orig_program.iter().cycle();
}

fn run_program(program: &mut std::iter::Cycle<core::slice::Iter<i32>>, original: &Vec<i32>) {
    match program.next().unwrap() {
        1 => {},
        2 => (),
        99 => (return),
        _ => (panic!("Not a viable operator!!")),
    }
}
