use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::{HashSet};

fn main() {
    process_input("Day13Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let mut points: HashSet<(i32,i32)> = HashSet::new();
    let mut first = false;
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let foldmode = line.chars().take(11).collect::<String>() == "fold along ".to_string();
        if line == "" {continue}
        if !foldmode {
            let parts: Vec<&str> = line.splitn(2,",").collect();
            let x: i32 = parts[0].parse().expect(format!("Bad format in x-coord on line {}", index).as_str());
            let y: i32 = parts[1].parse().expect(format!("Bad format in y-coord on line {}", index).as_str());
            points.insert((x,y));
        } else {
            let data = line[11..].to_string();
            let vertical: bool;
            match data.chars().nth(0).unwrap() {
                'y' => vertical = true,
                'x' => vertical = false,
                _ => panic!("Something went wrong"),
            }
            let at: i32 = data[2..].parse().expect(format!("Bad format in fold-at on line {}", index).as_str());
            if !first {fold(&mut points, at, vertical); first = true;}
        }
    }

    println!("{:?}", points);
    println!("{}", points.iter().count());
}

fn fold(points: &mut HashSet<(i32,i32)>, at: i32, vertical: bool) {
    for point in points.clone().iter() {
        if vertical {
            if point.1 > at {
                let new = (point.0, at - (at - point.1).abs());
                if !points.contains(&new) {points.insert(new);}
                points.remove(point);
            }
        } else {
            if point.0 > at {
                let new = (at - (at - point.0).abs(), point.1);
                if !points.contains(&new) {points.insert(new);}
                points.remove(point);
            }
        }
    }
}
