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
    
    let mut initial = lines.next().unwrap().expect("Could not read line 0");
    lines.next();
    for (index,line) in lines.enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let parts: Vec<&str> = line.splitn(2," -> ").collect();
        switches.insert(parts[0].to_string(),parts[1].to_string());
    }


    for _ in 0..10 {
        run_step(&mut initial, &switches);
    }
    println!("{}", score_polymer(&initial));
}

fn run_step(initial: &mut String, switches: &HashMap<String, String>) {
    let chars: Vec<char> = initial.chars().collect();
    let mut new: Vec<char> = vec![chars[0]];
    for l in 1..(chars.len()) {
        let pair = vec![chars[l-1], chars[l]].into_iter().collect::<String>();
        match switches.get(&pair) {
            Some(val) => {
                new.push(val.chars().nth(0).unwrap());
                new.push(chars[l]);
            },
            None => {
                new.push(chars[l]);
            },
        }
    }
    *initial = new.into_iter().collect();
}

fn score_polymer(polymer: &String) -> i32 {
    let chars: Vec<char> = polymer.chars().collect();
    let mut values: HashMap<char,i32> = HashMap::new();
    for chr in chars {
        let i = values.entry(chr).or_insert(1);
        *i += 1;
    }

    let mut max: (char,i32) = ('?', 0);
    let mut min: (char,i32) = ('?', i32::MAX);
    for (key,value) in values {
        if value > max.1 {max = (key.clone(), value)}
        if value < min.1 {min = (key.clone(), value)}
    }

    println!("{:?},{:?}", max,min);
    return max.1 - min.1;
}
