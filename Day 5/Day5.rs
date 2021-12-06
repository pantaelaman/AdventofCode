use std::fs::File;
use std::io::{self, BufRead, BufReader};

#[derive (Copy, Clone, Debug)]
struct Coordinate {
    x: i32,
    y: i32,
}

impl Coordinate {
    pub fn new(x: i32, y: i32) -> Coordinate {
        Coordinate{x, y}
    }
}

impl std::cmp::PartialEq for Coordinate {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
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
        let x_sub: bool;
        let y_sub: bool;
        if high.x > low.x {x_sub = false} else {x_sub = true}
        if high.y > low.y {y_sub = false} else {y_sub = true}
        if (low.x-high.x).abs() == (low.y-high.y).abs() {
            for x in 0..((high.x-low.x).abs()+1) {
                for y in 0..((high.y-low.y).abs()+1) {
                    if x != y {continue}
                    if x_sub {
                        if y_sub {
                            points.push(Coordinate::new(low.x-x, low.y-y));
                        } else {
                            points.push(Coordinate::new(low.x-x, low.y+y));
                        }
                    } else {
                        if y_sub {
                            points.push(Coordinate::new(low.x+x, low.y-y));
                        } else {
                            points.push(Coordinate::new(low.x+x, low.y+y));
                        }
                    }
                }
            }
        } else {
            for x in 0..((high.x-low.x).abs()+1) {
                for y in 0..((high.y-low.y).abs()+1) {
                    if x_sub {
                        if y_sub {
                            points.push(Coordinate::new(low.x-x, low.y-y));
                        } else {
                            points.push(Coordinate::new(low.x-x, low.y+y));
                        }
                    } else {
                        if y_sub {
                            points.push(Coordinate::new(low.x+x, low.y-y));
                        } else {
                            points.push(Coordinate::new(low.x+x, low.y+y));
                        }
                    }
                }
            }
        }
        Vent{low, high, points}
    }
}

impl std::cmp::PartialEq for Vent {
    fn eq(&self, other: &Self) -> bool {
        self.low == other.low && self.high == other.high
    }
}

fn main () {
    let vents = process_input("Day5Input.txt");

    println!("{} valid vents found", vents.len());
    let mut overlaps: Vec<Coordinate> = Vec::new();
    let mut grid: Vec<Vec<i32>> = vec![vec![0;1000];1000];
    for (index,vent) in vents.iter().enumerate() {
        draw_line(&mut grid, vent);
    }
    let mut overlaps: i32 = 0;
    for (i,col) in grid.iter().enumerate() {
        for (j,point) in col.iter().enumerate() {
            if *point > 1 {overlaps += 1}
        }
    }
    println!("Diag: {:#?}", vents[1]);
    println!("Overlaps: {}", overlaps);
}

fn draw_line(grid: &mut Vec<Vec<i32>>, vent: &Vent) {
    for (_,point) in vent.points.iter().enumerate() {
        grid[point.x as usize][point.y as usize] += 1;
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
        if !(low.x == high.x || low.y == high.y || (low.x-high.x).abs() == (low.y-high.y).abs()) {println!("Found invalid diagonal on line {}", index); continue}
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
