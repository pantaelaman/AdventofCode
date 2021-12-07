use std::fs::File;
use std::io::{self,BufRead,BufReader};

fn main() {

}

fn process_input(file: &str) -> i32 {
    let file = File::open(file).expect("Bad path");
    let mut valid = 0;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let parts: Vec<&str> = line.split(": ").collect();
        if parts.len() != 2 {panic!("Bad format on line {}", index)}
    }
}
