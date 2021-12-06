use std::fs::File;
use std::io::{self, BufRead, BufReader};

#[derive (Copy, Clone, Debug, Eq)]
struct Coordinate {
    x: i32,
    y: i32,
}

impl Coordinate {
    pub fn new(x: i32, y: i32) -> Coordinate {
        Coordinate{x, y}
    }
}

#[derive (Clone, Debug)]
struct Vent {
    low: Coordinate,
    high: Coordinate,
    points: Vec<Coordinate>,
}

impl Vent {
    pub fn new(low: Coordinate, high: Coordinate) -> Vent {
        let mut points: Vec<Coordinate> = Vec::new();
        let x_top: i32;
        let y_top: i32;
        for x in 0..((high.x-low.y).abs()+1) {
            for y in 0..((high.y-low.y).abs()+1) {
                points.push(Coordinate::new(low.x+x, low.y+y));
            }
        }
        Vent{low, high, points}
    }
}

fn main () {
    let vents = process_input("Day5Input.txt");
    let active_vents = vents.clone();
    println!("Vent example: {:#?}", vents.last().unwrap());
    println!("{} valid vents found", vents.len());
    let overlaps: Vec<Coordinate> = Vec::new();
    for (i,vent) in vents.iter().enumerate() {
        for (j,svent) in active_vents.iter().enumerate() {
            for (k,p1) in vent.points.iter().enumerate() {
                for (l,p2) in svent.points.iter().enumerate() {
                    if p1 == p2 && !overlaps.contains(p1) {
                        overlaps.push(p1);
                    }
                }
            }
        }
        
    }
}

fn process_input(file: &str) -> Vec<Vent> {
    let file = File::open(file).expect("Bad path");
    let mut vents: Vec<Vent> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let coords: Vec<&str> = line.as_str().split(" -> ").collect();
        if coords.len() != 2 {println!("Bad formatting on line {}", index); continue}
        let low = parse_coordinate(coords[0]).expect(format!("Failed to read low coordinate on line {}", index).as_str());
        let high = parse_coordinate(coords[1]).expect(format!("Failed to read high coordinate on line {}", index).as_str());
        if !(low.x == high.x || low.y == high.y) {println!("Found diagonal on line {}", index); continue}
        vents.push(Vent::new(low, high));
    }

    return vents;
}

fn parse_coordinate(input: &str) -> Result<Coordinate, &str> {
    let numbers: Vec<&str> = input.split(",").collect();
    if numbers.len() != 2 {return Err("Incorrect formatting of coordinate.")}
    let x = numbers[0].parse::<i32>().expect("Could not read x-value of coordinate.");
    let y = numbers[1].parse::<i32>().expect("Could not read y-value of coordinate.");
    return Ok(Coordinate::new(x,y));
}
