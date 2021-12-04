use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() {
    let data = process_input("Day3Input.txt");
    let oxygen = process_oxygen_data(data.clone());
    let co2 = process_co2_data(data.clone());
    println!("Oxygen: {}", oxygen);
    println!("CO2: {}", co2);
    println!("Answer: {}", oxygen*co2);
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

fn count_majority(data: Vec<Vec<u8>>) -> Vec<u8> {
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
        if *value as f32 >= (data.len() as f32)/2.0 {ret.push(1)}
        else {ret.push(0)}
    }

    return ret;
}

fn process_data(data: Vec<Vec<u8>>) -> (u32, u32) {
    let mut ret: Vec<u8> = count_majority(data.clone());

    let mut proc: Vec<u32> = vec![0; data.last().expect("Something went wrong.").len()];
    for (i,value) in ret.iter().enumerate() {
        proc[i] = (*value as u32) << ret.len()-1-i;
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

fn process_oxygen_data(data: Vec<Vec<u8>>) -> u32 {
    println!("-- Oxygen --");
    let mut mod_data = data.clone();

    for (i,_) in data.last().expect("Something went wrong.").iter().enumerate() {
        let majority = count_majority(mod_data.clone());
        println!("Round {}: {:#?}", i, mod_data);
        println!("Majority {}: {}", i, majority[i]);
        let mut new_data: Vec<Vec<u8>> = Vec::new();
        for (j,line) in mod_data.iter().enumerate() {
            if line[i] == majority[i] {
                new_data.push(line.clone());
            }
        }
        mod_data = new_data;
        if mod_data.len() == 1 {break}
    }

    let ret: Vec<u8> = (&mod_data[0]).clone();

    let mut proc: Vec<u32> = vec![0; data.last().expect("Something went wrong.").len()];
    for (i,value) in ret.iter().enumerate() {
        proc[i] = (*value as u32) << ret.len()-1-i;
        println!("V: {}, P: {}", value, proc[i]);
    }

    let mut oxygen: u32 = 0b00;
    for (i,value) in proc.iter().enumerate() {
        oxygen = oxygen | (*value as u32);
    }

    return oxygen;
}

fn process_co2_data(data: Vec<Vec<u8>>) -> u32 {
    println!("-- CO2 --");
    let mut mod_data = data.clone();

    for (i,_) in data.last().expect("Something went wrong.").iter().enumerate() {
        let majority = count_majority(mod_data.clone());
        println!("Round {}: {:#?}", i, mod_data);
        println!("Majority {}: {}", i, majority[i]);
        let mut new_data: Vec<Vec<u8>> = Vec::new();
        for (j,line) in mod_data.iter().enumerate() {
            if line[i] != majority[i] {
                new_data.push(line.clone());
            }
        }
        mod_data = new_data;
        if mod_data.len() == 1 {break}
    }

    let ret: Vec<u8> = (&mod_data[0]).clone();

    let mut proc: Vec<u32> = vec![0; data.last().expect("Something went wrong.").len()];
    for (i,value) in ret.iter().enumerate() {
        proc[i] = (*value as u32) << ret.len()-1-i;
        println!("V: {}, P: {}", value, proc[i]);
    }

    let mut co2: u32 = 0b00;
    for (i,value) in proc.iter().enumerate() {
        co2 = co2 | (*value as u32);
    }

    return co2;
}
