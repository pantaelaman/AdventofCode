use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::HashMap;

fn main() {
    process_input("Day12Test0.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    // HashMap<caveName, Vec<caveNames which are connected>>
    let mut caves: HashMap<String, Vec<String>> = HashMap::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let parts: Vec<&str> = line.splitn(2,"-").collect();
        
        match caves.get_mut(&*parts[0]) {
            Some(value) => value.push(parts[1].to_string()),
            None => {caves.insert(parts[0].to_string(), vec![parts[1].to_string()]);}, // Semicolon necessary for same return type, apparently
        };
        match caves.get_mut(&*parts[1]) {
            Some(value) => value.push(parts[0].to_string()),
            None => {caves.insert(parts[1].to_string(), vec![parts[0].to_string()]);},
        };
    }

    println!("{:?}", caves);
}
