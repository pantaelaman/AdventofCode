use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    process_input("Day3Test1.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);

    let mut lines = reader.lines().collect::<Vec<Result<String, std::io::Error>>>();
    let mut lines = lines.iter_mut();

    let line1 = lines.nth(0).unwrap().as_ref().expect("Could not read line 0").split(",").scan((0_i32,0_i32),|pos,el| {
        let vertical = match el.chars().nth(0).unwrap() {
            'U' => true,
            'D' => true,
            'L' => false,
            'R' => false,
            _ => panic!("Something went wrong"),
        };
        let amplitude = el.chars().skip(1).collect::<String>();
        println!("{}", amplitude);
        let amplitude = amplitude.parse::<i32>().expect("Could not parse amplitude");
        let mut ret: Vec<(i32,i32)> = Vec::new();
        if vertical {
            for y in (pos.1)..(pos.1 + amplitude + 1) {
                ret.push((pos.0,y));
            }
            pos.1 = pos.1 + amplitude;
        } else {
            for x in (pos.0)..(pos.0 + amplitude + 1) {
                ret.push((x,pos.1))
            }
            pos.0 = pos.0 + amplitude;
        }
        Some(ret)
    }).fold(Vec::new(), |mut acc,mut el| {
        acc.append(&mut el);
        acc
    });
    println!("{:?}", line1);
    let line2 = lines.nth(0).unwrap().as_ref().expect("Could not read line 1");
}
