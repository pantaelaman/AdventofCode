use std::fs::File;
use std::io::{BufRead,BufReader};

fn main() {
    println!("Test: {}", process_input("Day1test.txt"));
    println!("Result: {}", process_input("Day1input.txt"));
}

fn process_input(file: &str) -> i32 {
    let file = File::open(file).expect("Bad path");
    let mut floor: i32 = 0;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        for (cindex,chr) in line.chars().enumerate() {
            match chr {
                '(' => floor+=1,
                ')' => floor-=1,
                _ => panic!("Unexpected character in input file"),
            }
            if floor < 0 {return (cindex+1) as i32}
        }
    }
    return -1;
}

