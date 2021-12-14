use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::HashMap;

fn main() {
    process_input("Day14Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let mut switches: HashMap<String, String> = HashMap::new();
    let mut lines = BufReader::new(file).lines();
    
    let raw = lines.next().unwrap().expect("Could not read line 0");
    let mut polymer = build_polymer(&raw);
    lines.next();
    for (index,line) in lines.enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let parts: Vec<&str> = line.splitn(2," -> ").collect();
        switches.insert(parts[0].to_string(),parts[1].to_string());
    }

    let mut counter: HashMap<char,i64> = HashMap::new();
    count_initial(&raw, &mut counter);
    for _ in 0..40 {
        run_step(&mut polymer, &switches, &mut counter);
    }

    println!("{}", score_polymer(&counter));
}

fn count_initial(raw: &String, counter: &mut HashMap<char,i64>) {
    for chr in raw.chars() {
        *counter.entry(chr).or_insert(0) += 1;
    }
}

fn build_polymer(raw: &String) -> HashMap<String,i64> {
    let chars: Vec<char> = raw.chars().collect();
    let mut result: HashMap<String,i64> = HashMap::new();
    for l in 1..(chars.len()) {
        let pair = vec![chars[l-1],chars[l]].into_iter().collect::<String>();
        let counter = result.entry(pair).or_insert(0);
        *counter += 1;
    }
    return result;
}

fn run_step(polymer: &mut HashMap<String,i64>, switches: &HashMap<String, String>, counter: &mut HashMap<char,i64>) {
    let mut new = HashMap::new();
    for (key,value) in polymer.iter() {
        match switches.get(key) {
            Some(ltr) => {
                let c1 = new.entry(format!("{}{}", key.clone().chars().nth(0).unwrap(), ltr)).or_insert(0);
                *c1 += *value;
                let c2 = new.entry(format!("{}{}", ltr, key.clone().chars().last().unwrap())).or_insert(0);
                *c2 += *value;
                let m = counter.entry(ltr.chars().nth(0).unwrap()).or_insert(0);
                *m += *value;
            },
            None => (),
        }
    }
    *polymer = new;
}

fn score_polymer(counter: &HashMap<char,i64>) -> i64 {
    let mut max: (char,i64) = ('?',0);
    let mut min: (char,i64) = ('?',i64::MAX);
    for (key,value) in counter {
        if *value > max.1 {
            max = (key.clone(),*value);
        }
        if *value < min.1 {
            min = (key.clone(),*value);
        }
    }
    return max.1 - min.1;
}
