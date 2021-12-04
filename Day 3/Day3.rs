use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() {
    let (gamma, epsilon) = process_data(process_input("Day3Input.txt"));
    println!("Gamma: {}", gamma);
    println!("Epsilon: {}", epsilon);
    println!("Answer: {}", gamma*epsilon);
}

fn process_input(file: &str) -> Vec<Vec<u8>> {
    let file = File::open(file).expect("Bad path");
    let mut ret: Vec<Vec<u8>> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        ret.push(Vec::new());
        for (pos,chr) in line.chars().enumerate() {
            let value = chr.to_string().parse::<u8>().expect(format!("Could not parse char {} on line {}", pos, index).as_str());
            ret.last_mut().expect("Something went wrong.").push(value);
        }
    }

    return ret;
}

fn process_data(data: Vec<Vec<u8>>) -> (u32, u32) {
    let mut ret: Vec<u8> = Vec::new();
    let mut count: Vec<i32> = vec![0; data.last().expect("Something went wrong.").len()];

    for (i,line) in data.iter().enumerate() {
        for (j,digit) in line.iter().enumerate() {
            if *digit == 1 {
                count[j] += 1;
            }
        }
    }

    for (i,value) in count.iter().enumerate() {
        if *value as usize > data.len()/2 {ret.push(1)}
        else {ret.push(0)}
    }

    let mut proc: Vec<u32> = vec![0; data.last().expect("Something went wrong.").len()];
    for (i,value) in ret.iter().enumerate() {
        proc[i] = (*value as u32) << ret.len()-1-i;
        println!("{}", proc[i]);
    }
    let mut gamma: u32 = 0b00;
    for (i,value) in proc.iter().enumerate() {
        gamma = gamma | value;
    }
    let mut epsilon: u32 = !gamma;
    let mut proc: String = format!("{:b}", epsilon);
    proc = proc[proc.len()-data.last().expect("Something went wrong.").len()..].to_string();
    epsilon = u32::from_str_radix(proc.as_str(), 2).expect("Could not produce epsilon.");

    return (gamma, epsilon);
}
