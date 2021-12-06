use std::fs::File;
use std::io::{self, BufRead, BufReader};

const BIRTH_TIMER: i8 = 8;

#[derive (Clone, Debug)]
struct LanternFish {
    timer: i8,
}

impl LanternFish {
    fn new(timer: i8) -> LanternFish {
        LanternFish{timer}
    }
    fn update(&mut self) -> Option<LanternFish> {
        self.timer -= 1;
        if self.timer == -1 {
            self.timer = 6;
            return Some(LanternFish::new(BIRTH_TIMER));
        }
        return None;
    }
}

#[derive (Clone, Debug)]
struct FishController {
    timer: i8,
    count: i64,
}

impl FishController {
    pub fn new(timer: i8) -> FishController {
        FishController{timer, count: 0}
    }
}

fn main() {
    let mut fishes = process_input("Day6Input.txt");
    println!("Initial fishes: {:#?}", fishes);
    for day in 0..256 {
        let mut buffer: Vec<LanternFish> = Vec::new();
        for (index,fish) in fishes.iter_mut().enumerate() {
            match fish.update() {
                Some(baby) => buffer.push(baby),
                None => (),
            }
        }
        fishes.append(&mut buffer);
        println!("Completed day {}", day);
    }
    println!("Fishes remaining: {}", fishes.len());
}

fn process_input(file: &str) -> Vec<LanternFish> {
    let file = File::open(file).expect("Bad path");
    let mut fishes: Vec<LanternFish> = Vec::new(); // Get out, grammar nerds (myself included)
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        if index > 0 {break}
        let timers_raw: Vec<&str> = line.split(",").collect();
        let mut timers: Vec<i8> = Vec::new();
        for (i,raw) in timers_raw.iter().enumerate() {
            timers.push(raw.parse::<i8>().expect(format!("Bad format at fish {}", i).as_str()));
        }
        for (i,timer) in timers.iter().enumerate() {
            fishes.push(LanternFish::new(*timer));
        }
    }

    return fishes;
}
