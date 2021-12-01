use std::fs::File;
use std::io::{BufRead,BufReader};

fn main() {
    let data = process_input("Day1Input.txt");
    let mini = shrink_data(data);
    let hmap = process_data(mini);
    let result = process_hmap(hmap);
    println!("{}", result);
}

fn process_hmap(hmap: Vec<bool>) -> i32 {
    let mut ret: i32 = 0;

    for (_, value) in hmap.iter().enumerate() {
        if *value {ret += 1}
    }
    
    return ret;
}

fn process_data(data: Vec<i32>) -> Vec<bool> {
    let mut ret: Vec<bool> = Vec::new();
    
    for (index,value) in data.iter().enumerate() {
        if index == 0 {continue}
        ret.push(value > &data[index-1]);
    }

    return ret;
}

fn shrink_data(data: Vec<i32>) -> Vec<i32> {
    let mut ret: Vec<i32> = Vec::new();
    
    for (index,value) in data.iter().enumerate() {
        if index == 0 || index == 1 {continue}
        ret.push(value + &data[index-1] + &data[index-2]);
    }

    return ret;
}

fn process_input(file: &str) -> Vec<i32> {
    let file = File::open(file).expect("Bad path");
    let mut ret: Vec<i32> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let num = line.parse::<i32>().expect(format!("Could not parse line {}", index).as_str());
        ret.push(num);
    }

    return ret;
}
