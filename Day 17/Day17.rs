use std::fs::File;
use std::io::{BufRead,BufReader};

fn main() {
    process_input("Day17Test.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let input = BufReader::new(file).lines().nth(0).expect("Could not read line 0").unwrap();
    let parts = input.chars().skip(13).collect::<String>().split(", ").map(|el| {
        el.chars().skip(2).collect::<String>().split("..").map(|el| el.parse::<i32>().unwrap()).collect::<Vec<i32>>()
    }).collect::<Vec<Vec<i32>>>();
    println!("{:?}", parts);
}
