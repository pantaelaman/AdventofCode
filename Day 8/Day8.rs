use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() {
    println!("{}", process_input("Day8Input.txt"));
}

fn process_input(file: &str) -> i32 {
    let file = File::open(file).expect("Bad path");
    let mut value: i32 = 0;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        value += handle_line(line.as_str()).expect(format!("Error on line {}", index).as_str());
    }

    return value;
}

fn handle_line(line: &str) -> Result<i32, String> {
    let raw: Vec<&str> = line.split(" | ").collect();
    if raw.len() != 2 {return Err("Bad format".to_string())}
    return Ok(parse_output(raw[1]));
}

fn parse_output(raw: &str) -> i32 {
    let mut value: i32 = 0;
    let data: Vec<&str> = raw.split(" ").collect();
    for (index,datum) in data.iter().enumerate() {
        let length = datum.len();
        if (length == 2 || length == 3 || length == 4 || length == 7) {
            value += 1;
        }
    }
    return value;
}
