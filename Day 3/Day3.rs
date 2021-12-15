use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashSet;

fn main() {
    process_input("Day3Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect(format!("Bad path {}", file).as_str());
    let reader = BufReader::new(file);

    let mut lines = reader.lines().collect::<Vec<Result<String, std::io::Error>>>();
    let mut lines = lines.iter_mut();

    let mut line1 = lines.nth(0).unwrap().as_ref().expect("Could not read line 0").split(",").scan((0_i32,0_i32),|pos,el| process_line(pos, el));
    let mut line2 = lines.nth(0).unwrap().as_ref().expect("Could not read line 1").split(",").scan((0_i32,0_i32),|pos,el| process_line(pos, el));
    
    println!("{:#?}", line1);
}

fn process_line(pos: &mut (i32,i32), el: &str) -> Option<((i32,i32),(i32,i32))> {
    let value: i32 = el[1..].parse().unwrap();
    let curpos = pos.clone();
    match el.chars().nth(0).unwrap() {
        'U' => {*pos = (pos.0,pos.1-value); return Some((curpos,pos.clone()));},
        'D' => {*pos = (pos.0,pos.1+value); return Some((curpos,pos.clone()));},
        'L' => {*pos = (pos.0-value,pos.1); return Some((curpos,pos.clone()));},
        'R' => {*pos = (pos.0+value,pos.1); return Some((curpos,pos.clone()));},
    }
}