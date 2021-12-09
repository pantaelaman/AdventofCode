use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() {
    println!("{}", process_input("Day8Input.txt"));
}

fn process_input(file: &str) -> i64 {
    let file = File::open(file).expect("Bad path");
    let mut value: i64 = 0;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        println!("Line {}", index);
        value += handle_line(line.as_str()).expect(format!("Error on line {}", index).as_str());
    }

    return value;
}

fn handle_line(line: &str) -> Result<i64, String> {
    let raw: Vec<&str> = line.split(" | ").collect();
    if raw.len() != 2 {return Err("Bad format".to_string())}
    return Ok(parse_output(raw[1], parse_input(raw[0])));
}

fn parse_output(raw: &str, key: [Vec<char>; 10]) -> i64 {
    let mut value: i64 = 0;
    let data: Vec<&str> = raw.split(" ").collect();
    let length = (data.len() - 1) as i64;
    for (index,datum) in data.iter().enumerate() {
        let mut chrs = datum.chars().collect::<Vec<char>>();
        chrs.sort();
        for (j,point) in key.iter().enumerate() {
            if chrs.iter().collect::<String>() == point.iter().collect::<String>() {
                let result = (j as i64) * (10_i64.pow((length - index as i64) as u32));
                value += result;
                break;
            }
        }
    }
    println!("{}", value);
    return value;
}

fn parse_input(raw: &str) -> [Vec<char>; 10] {
    let parts: Vec<&str> = raw.split(" ").collect();
    
    //  aa
    // b  c
    // b  c
    //  dd 
    // e  f
    // e  f
    //  gg 
    
    let one = parts.iter().find(|part| part.chars().count() == 2).unwrap();
    let four = parts.iter().find(|part| part.chars().count() == 4).unwrap();
    let seven = parts.iter().find(|part| part.chars().count() == 3).unwrap();
    let eight = parts.iter().find(|part| part.chars().count() == 7).unwrap();
    let lseg = four.chars().filter(|x| !one.contains(*x));
    let nine = parts.iter().filter(|part| part.chars().count() == 6).find(|x| four.chars().all(|y| x.contains(y))).unwrap();
    let zero = parts.iter() .filter(|part| part.chars().count() == 6).find(|x| !lseg.clone().all(|y| x.contains(y))).unwrap();
    let six = parts.iter().filter(|part| part.chars().count() == 6).find(|x| x != &nine && x != &zero).unwrap();
    let three = parts.iter().filter(|part| part.chars().count() == 5).find(|x| one.chars().all(|y| x.contains(y))).unwrap();
    let five = parts.iter().filter(|part| part.chars().count() == 5).find(|x| lseg.clone().all(|y| x.contains(y))).unwrap();
    let two = parts.iter().filter(|part| part.chars().count() == 5).find(|x| x != &three && x != &five).unwrap();

    let mut zero = zero.chars().collect::<Vec<char>>();
    let mut one = one.chars().collect::<Vec<char>>();
    let mut two = two.chars().collect::<Vec<char>>();
    let mut three = three.chars().collect::<Vec<char>>();
    let mut four = four.chars().collect::<Vec<char>>();
    let mut five = five.chars().collect::<Vec<char>>();
    let mut six = six.chars().collect::<Vec<char>>();
    let mut seven = seven.chars().collect::<Vec<char>>();
    let mut eight = eight.chars().collect::<Vec<char>>();
    let mut nine = nine.chars().collect::<Vec<char>>();

    zero.sort();
    one.sort();
    two.sort();
    three.sort();
    four.sort();
    five.sort();
    six.sort();
    seven.sort();
    eight.sort();
    nine.sort();

    return [zero,one,two,three,four,five,six,seven,eight,nine];
}
