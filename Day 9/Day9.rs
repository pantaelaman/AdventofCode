use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    println!("{}", process_input("Day9Input.txt"));
}

fn process_input(file: &str) -> i64 {
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

    let lowest = find_lowest_points(map.clone());
    let mut exclude: Vec<(usize,usize)> = Vec::new();
    let mut areas = lowest.iter().map(|val| (cover_area(
        &map,
        *val,
        &mut exclude,
        0,
    ))).collect::<Vec<i32>>();
    println!("{:?}", areas);
    let g1 = areas.iter().reduce(|a,b| if a > b { a } else { b }).unwrap();
    let mut nareas = areas.clone();
    nareas.swap_remove(nareas.iter().position(|x| x == g1).unwrap());
    let g2 = nareas.iter().reduce(|a,b| if a > b { a } else { b }).unwrap();
    let mut oareas = nareas.clone();
    oareas.swap_remove(oareas.iter().position(|x| x == g2).unwrap());
    let g3 = oareas.iter().reduce(|a,b| if a > b { a } else { b }).unwrap();
    return *g1 as i64 * *g2 as i64 * *g3 as i64;
}

fn find_lowest_points(map: Vec<Vec<i8>>) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize,usize)> = Vec::new();

    // Check center
    let bridge: [i8; 2] = [-1,1];
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
            if passed {result.push((x,y))}
        }
    }

    return result;
}


fn cover_area(map: &Vec<Vec<i8>>, at: (usize,usize), exclude: &mut Vec<(usize,usize)>, acc: i32) -> i32 {
    let mut acc = acc + 1;
    
    if !(exclude.contains(&at)) {exclude.push(at.clone())}

    // Neighbor +1x
    if !(at.0 + 1 >= map[at.1].len()) && !exclude.contains(&(at.0 + 1,at.1)) {
        exclude.push((at.0 + 1,at.1));
        if map[at.1][at.0 + 1] != 9 {
            acc = cover_area(map, (at.0 + 1,at.1), exclude, acc);
        }
    }

    // Neighbor -1x
    if !(at.0 == 0) && !exclude.contains(&(at.0 - 1,at.1)) {
        exclude.push((at.0 - 1,at.1));
        if map[at.1][at.0 - 1] != 9 {
            acc = cover_area(map, (at.0 - 1,at.1), exclude, acc);
        }
    }

    // Neighbor +1y
    if !(at.1 + 1 >= map.len()) && !exclude.contains(&(at.0,at.1 + 1)) {
        exclude.push((at.0,at.1 + 1));
        if map[at.1 + 1][at.0] != 9 {
            acc = cover_area(map, (at.0,at.1 + 1), exclude, acc);
        }
    }

    // Neighbor -1y
    if !(at.1 == 0) && !exclude.contains(&(at.0,at.1 - 1)) {
        exclude.push((at.0,at.1 - 1));
        if map[at.1 - 1][at.0] != 9 {
            acc = cover_area(map, (at.0,at.1 - 1), exclude, acc);
        }
    }

    return acc;
}