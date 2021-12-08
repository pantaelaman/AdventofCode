use std::fs::File;
use std::io::{self,BufRead,BufReader};

struct Requirement {
    chr: char,
    min: i32,
    max: i32,
}

fn main() {
    println!("Result: {}", process_input("Day2Test.txt"));
}

fn process_input(file: &str) -> i32 {
    let file = File::open(file).expect("Bad path");
    let mut valid = 0;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let parts: Vec<&str> = line.split(": ").collect();
        if parts.len() != 2 {panic!("Bad format on line {}", index)}
        if password_match(parts[1], parse_requirements(parts[0]).expect(format!("Could not parse requirement on line {}", index).as_str())) {valid += 1}
        println!("Checked password {}", index);
    }

    return valid;
}

fn parse_requirements(raw: &str) -> Result<Requirement, &str> {
    let parts: Vec<&str> = raw.split(" ").collect();
    if parts.len() != 2 {return Err("Bad format for requirement")}
    let range: Vec<&str> = parts[0].split("-").collect();
    if range.len() != 2 {return Err("Bad format for range")}
    let chr: char = match parts[1].chars().nth(0) {
        Some(value) => value,
        None => return Err("Invalid format for char"),
    };
    let min = match range[0].parse::<i32>() {
        Ok(value) => value,
        Err(_) => return Err("Invalid format for minimum"),
    };
    let max = match range[1].parse::<i32>() {
        Ok(value) => value,
        Err(_) => return Err("Invalid format for maximum"),
    };
    Ok(Requirement{chr,min,max})
}

fn password_match(pword: &str, requirement: Requirement) -> bool {
    let count = pword.matches(&requirement.chr.to_string()).count();
    count > requirement.min as usize && count < requirement.max as usize
}
