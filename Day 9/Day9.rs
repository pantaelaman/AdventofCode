use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    println!("{}", process_input("Day9Input.txt"))
}

fn process_input(file: &str) -> i32 {
    let file = File::open(file).expect("Bad path");
    let mut map: Vec<Vec<i8>> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let mut row: Vec<i8> = Vec::new();
        for (x,chr) in line.chars().enumerate() {
            row.push(chr.to_string().parse::<i8>().expect(format!("Could not parse char {} on line {}", x, index).as_str()));
        }
        map.push(row);
    }

    let lowest = find_lowest_points(map);
    println!("{:#?}", lowest);
    let result: i32 = lowest.iter().fold(0_i32,|acc,el| 1 + acc + *el as i32);
    return result;
}

fn find_lowest_points(map: Vec<Vec<i8>>) -> Vec<i8> {
    let mut result: Vec<i8> = Vec::new();

    // Check center
    let bridge: [i8; 2] = [-1,1];
    let mut debug = 0;
    for y in 0..(map.len()) {
        for x in 0..(map[y].len()) {
            let point = map[y][x];
            let mut passed: bool = true;
            for (_,i) in bridge.iter().enumerate() {
                if (x as i8 + i) < 0 || (x as i8 + i) >= map[y].len() as i8 {continue}
                let neighbor = map[y][(x as i8 +i) as usize];
                if neighbor <= point {passed = false; break}
            }
            for (_,j) in bridge.iter().enumerate() {
                if (y as i8 + j) < 0 || (y as i8 + j) >= map.len() as i8 {continue}
                let neighbor = map[(y as i8 +j) as usize][x];
                if neighbor <= point {passed = false; break}
            }
            debug += 1;
            if passed {result.push(point)}
            if point == 9 {println!("9 at {},{}", x,y)}
        }
    }
    println!("{}", debug);

    return result;
}
