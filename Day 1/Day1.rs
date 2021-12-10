use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    println!("{}", process_input("Day1Input.txt"));
}

fn process_input(file: &str) -> i64 {
    let file = File::open(file).expect("Bad path");
    let mut fuel: i64 = 0;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let value = line.parse::<i64>().expect(format!("Could not parse line {}", index).as_str());

        fuel += (value / 3) - 2;
    }

    return fuel;
}
