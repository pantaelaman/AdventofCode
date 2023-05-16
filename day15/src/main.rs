#![allow(dead_code)]
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct Sensor {
    x: i32,
    y: i32,
    c_beacon: Beacon,
}

impl Sensor {
    fn get_max_distance(&self) -> i32 {
        (self.c_beacon.x - self.x).abs() + (self.c_beacon.y - self.y).abs()
    }

    fn get_max_up_down(&self) -> (i32, i32) {
        let distance = self.get_max_distance();
        (self.y + distance, self.y - distance)
    }

    fn is_within_rows_range(&self, row: i32) -> bool {
        let max_up_down = self.get_max_up_down();
        (max_up_down.1..=max_up_down.0).contains(&row)
    }

    fn is_point_within_range(&self, x: i32, y: i32) -> bool {
        self.get_max_distance() >= (x - self.x).abs() + (y - self.y).abs()
    }

    fn get_line_intercepts(&self) -> (i32, i32, i32, i32) {
        let dist = self.get_max_distance();
        let ur = self.y + self.x + dist;
        let dl = self.y + self.x - dist;
        let ul = self.y - self.x + dist;
        let dr = self.y - self.x - dist;
        (ur, ul, dl, dr)
    }
}

#[derive(Debug)]
struct Beacon {
    x: i32,
    y: i32,
}

fn read_sensors(filename: &str) -> Vec<Sensor> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    let mut sensors = Vec::new();
    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split(": ").collect();
        let sensor_parts: Vec<i32> = parts[0]
            .split("at ")
            .skip(1)
            .next()
            .unwrap()
            .split(", ")
            .map(|s| s.split("=").skip(1).next().unwrap())
            .map(|n| n.parse().unwrap())
            .collect();
        let beacon_parts: Vec<i32> = parts[1]
            .split("at ")
            .skip(1)
            .next()
            .unwrap()
            .split(", ")
            .map(|s| s.split("=").skip(1).next().unwrap())
            .map(|n| n.parse().unwrap())
            .collect();

        sensors.push(Sensor {
            x: sensor_parts[0],
            y: sensor_parts[1],
            c_beacon: Beacon {
                x: beacon_parts[0],
                y: beacon_parts[1],
            },
        });
    }

    sensors
}

const TARGET_ROW: i32 = 10;
const SIZE: i32 = 20;

fn main() {
    let sensors = read_sensors("test.txt");

    let reachable_sensors: Vec<&Sensor> = sensors
        .iter()
        .filter(|s| s.is_within_rows_range(TARGET_ROW))
        .collect();

    let mut invalid_x: HashSet<i32> = HashSet::new();
    for sensor in reachable_sensors {
        let max_dist = sensor.get_max_distance();
        let dist_y = (TARGET_ROW - sensor.y).abs();
        let dist_x = max_dist - dist_y;
        for x in (sensor.x - dist_x)..=(sensor.x + dist_x) {
            if sensor.c_beacon.y == TARGET_ROW && sensor.c_beacon.x == x {
                continue;
            }
            invalid_x.insert(x);
        }
    }

    println!("Part 1: {}", invalid_x.len());

    let neg_lines: Vec<i32> = sensors
        .iter()
        .map(|s| {
            let intercepts = s.get_line_intercepts();
            [intercepts.0, intercepts.2]
        })
        .flatten()
        .collect();
    let pos_lines: Vec<i32> = sensors
        .iter()
        .map(|s| {
            let intercepts = s.get_line_intercepts();
            [intercepts.1, intercepts.3]
        })
        .flatten()
        .collect();

    println!("Neg: {:?}", neg_lines);
    println!("Pos: {:?}", pos_lines);

    let mut neg_pairs = HashSet::new();
    for line in neg_lines.iter() {
        neg_lines
            .iter()
            .filter_map(|i| {
                if (line - *i).abs() == 2 {
                    Some((*line, *i))
                } else {
                    None
                }
            })
            .for_each(|p| {
                neg_pairs.insert(p.clone());
            });
    }

    let mut pos_pairs = HashSet::new();
    for line in pos_lines.iter() {
        pos_lines
            .iter()
            .filter_map(|i| {
                if (line - *i).abs() == 2 {
                    Some((*line, *i))
                } else {
                    None
                }
            })
            .for_each(|p| {
                pos_pairs.insert(p.clone());
            });
    }

    println!("Neg Pairs: {:?}", neg_pairs);
    println!("Pos Pairs: {:?}", pos_pairs);

    let mut true_quads = HashSet::new();
    for neg_pair in neg_pairs {
        pos_pairs.iter().for_each(|pos_pair| {
            let (neg_b, neg_s) = (neg_pair.0.max(neg_pair.1), neg_pair.0.min(neg_pair.1));
            let (pos_b, pos_s) = (pos_pair.0.max(pos_pair.1), pos_pair.0.min(pos_pair.1));

            if (neg_b - pos_s).abs() - (neg_s - pos_b).abs() == 4 {
                true_quads.insert((neg_b, pos_b, neg_s, pos_s));
            }
        });
    }

    println!("Quads: {:?}", true_quads);

    let points: Vec<(i32, i32)> = true_quads
        .iter()
        .filter_map(|true_quad| {
            let y = (true_quad.0 + true_quad.3) / 2;
            let x = true_quad.0 - y - 1;
            if x > SIZE || y > SIZE || x < 0 || y < 0 {
                None
            } else {
                Some((x, y))
            }
        })
        .collect();

    println!("Points: {:?}", points);
}
