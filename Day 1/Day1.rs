use std::fs::File;
use std::io::{self,BufRead,BufReader};

fn main () {
    let result = process_data(process_input("Day1Input.txt")).expect("Err while processing");
    println!("Result: {}", result);
}

fn process_input(file: &str) -> Vec<i32> {
    let file = File::open(file).expect("Bad path");
    let mut expenses: Vec<i32> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());

        expenses.push(line.parse::<i32>().expect(format!("Could not parse value on line {}", index).as_str()));
    }

    return expenses;
}

fn process_data(data: Vec<i32>) -> Result<i32, String> {
    for (i,datum) in data.iter().enumerate() {
        for (j,other) in data.iter().enumerate() {
            if (i==j) {continue}
            for (k,oother) in data.iter().enumerate() {
                if (i==j || k==j) {continue}
                if datum + other + oother == 2020 {return Ok(datum*other*oother)}
            }
        }
        println!("Processed expense {}", i);
    }
    return Err("Could not find a match".to_string());
}
