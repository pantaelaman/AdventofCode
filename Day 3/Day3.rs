use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    process_input("Day3Test0.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);

    let mut lines = reader.lines().collect::<Vec<Result<String, std::io::Error>>>();
    let mut lines = lines.iter_mut();

    let line1 = lines.nth(0).unwrap().as_ref().expect("Could not read line 0").split(",").scan((0_i32,0_i32),|pos,el| {
        create_line(pos,el);
    }).fold(Vec::new(), |mut acc,mut el| {
        acc.append(&mut el);
        acc
    });
    let line2 = lines.nth(0).unwrap().as_ref().expect("Could not read line 1").split(",").scan((0_i32,0_i32),|pos,el| {
        create_line(pos,el);
    }).fold(Vec::new(), |mut acc,mut el| {
        acc.append(&mut el);
        acc
    });
    let intersections = line1.iter().filter(|el| line2.iter().any(|sel| *el == sel)).cloned().collect::<Vec<(i32,i32)>>();
    println!("{:?}", line1);
    println!("{:?}", line2)
}

fn create_line(pos: (i32,i32), el: &str) -> Option<Vec<(i32,i32)>> {
    let vertical = match el.chars().nth(0).unwrap() {
        'U' => true,
        'D' => true,
        'L' => false,
        'R' => false,
        _ => panic!("Something went wrong"),
    };
    let amplitude = el.chars().skip(1).collect::<String>();
    let amplitude = amplitude.parse::<i32>().expect("Could not parse amplitude") * match el.chars().nth(0).unwrap() {
        'U' => 1,
        'D' => -1,
        'L' => -1,
        'R' => 1,
        _ => panic!("Something went wrong"),
    };
    let mut ret: Vec<(i32,i32)> = Vec::new();
    if vertical {
        for y in 0..amplitude {
            let val = y * match el.chars().nth(0).unwrap() {
                'U' => 1,
                'D' => -1,
                'L' => -1,
                'R' => 1,
                _ => panic!("Something went wrong"),
            };
            ret.push((pos.0,pos.1 + val));
        }
        pos.1 = pos.1 + amplitude;
    } else {
        for x in 0..amplitude {
            let val = x * match el.chars().nth(0).unwrap() {
                'U' => 1,
                'D' => -1,
                'L' => -1,
                'R' => 1,
                _ => panic!("Something went wrong"),
            };
            ret.push((pos.0 + val,pos.1))
        }
        pos.0 = pos.0 + amplitude;
    }
    Some(ret)
}
