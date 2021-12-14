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

    let mut line1 = lines.nth(0).unwrap().as_ref().expect("Could not read line 0").split(",").scan((0_i32,0_i32),|pos,el| process_line(pos, el)).fold(Vec::new(), |mut acc,mut el| {
        acc.append(&mut el);
        acc
    });
    let mut line2 = lines.nth(0).unwrap().as_ref().expect("Could not read line 1").split(",").scan((0_i32,0_i32),|pos,el| process_line(pos, el)).fold(Vec::new(), |mut acc,mut el| {
        acc.append(&mut el);
        acc
    });

    line1.sort();
    line2.sort();
    
    let mut count = 0;
    let mut total = line1.len();
    'main: for i in line1 {
        for j in &line2 {
            if i == *j {
                println!("Distance: {}", i.0.abs() + i.1.abs());
                break 'main;
            }
            if j.0 > i.0 || j.1 > i.1 {break}
        }
        println!("Checked major {} of {}", count, total);
        count += 1;
    }
}

fn process_line(pos: &mut (i32,i32), el: &str) -> Option<Vec<(i32,i32)>> {
    let mut points: Vec<(i32,i32)> = Vec::new();
    let amplitude = (match el.chars().nth(0).unwrap() {
        'L' | 'R' => false,
        'U' | 'D' => true,
        _ => panic!("Unexpected character"),
    }, el[1..].parse::<i32>().unwrap(), match el.chars().nth(0).unwrap() {
        'L' | 'D' => -1,
        'R' | 'U' => 1,
        _ => panic!("Unexpected character"),
    });
    if amplitude.0 {
        for n in 0..(amplitude.1) {
            points.push((pos.0, pos.1 + (n * amplitude.2)));
        }
        pos.1 += amplitude.1 * amplitude.2;
    } else {
        for n in 0..(amplitude.1) {
            points.push((pos.0 + (n * amplitude.2), pos.1));
        }
        pos.0 += amplitude.1 * amplitude.2;
    }
    return Some(points);
}

fn calculate_shortest_distance(intersections: &HashSet<(i32,i32)>) -> i32 {
    let mut min = i32::MAX;
    for intersection in intersections {
        let distance = intersection.0.abs() + intersection.1.abs();
        if distance < min {min = distance;}
    }
    return min;
}
