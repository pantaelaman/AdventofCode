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
    let fishes = process_input("Day6Input.txt");
    println!("Initial fishes: {:#?}", fishes);
    let mut controllers = build_fish_controllers(fishes);
    for day in 0..256 {
        let mut new = build_fish_controllers(Vec::new());
        for (timer,controller) in controllers.iter_mut().enumerate() {
            if timer == 0 {
                new[8].count += controller.count;
                new[6].count += controller.count;
            } else {
                new[timer-1].count += controller.count;
            }
        }
        controllers = new;
        println!("Finished day {}", day);
    }
    println!("Final fishes: {}", calculate_final(controllers));
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

fn build_fish_controllers(fishes: Vec<LanternFish>) -> [FishController; 9] {
    let mut controllers: [FishController; 9] = [
        FishController::new(0),
        FishController::new(1),
        FishController::new(2),
        FishController::new(3),
        FishController::new(4),
        FishController::new(5),
        FishController::new(6),
        FishController::new(7),
        FishController::new(8),
    ];
    for (index,fish) in fishes.iter().enumerate() {
        controllers[fish.timer as usize].count += 1;
    }
    return controllers;
}

fn calculate_final(controllers: [FishController; 9]) -> i64 {
    let mut total: i64 = 0;
    for controller in controllers {
        total += controller.count;
    }
    return total;
}
