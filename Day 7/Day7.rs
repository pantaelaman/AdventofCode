use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() {
    let (controllers, min, max) = process_input("Day7Input.txt");
    println!("Result: {:?}", run_tests(controllers, min, max));
}

fn process_input(file: &str) -> (Vec<(i32,i32)>, i32, i32) {
    let file = File::open(file).expect("Bad path");
    let mut controllers: Vec<(i32,i32)> = Vec::new();
    let reader = BufReader::new(file);
    let mut max: i32 = 0;
    let mut min: i32 = 2_i32.pow(16);
    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        for (i,raw) in line.split(",").enumerate() {
            let value: i32 = raw.parse::<i32>().expect(format!("Could not read value {}", i).as_str());
            if value > max { max = value }
            if value < min { min = value }
            match controllers.iter_mut().find(|x| x.0 == value) {
                Some(x) => (x.1 += 1),
                None => controllers.push((value,1)),
            }
        }
    }
    return (controllers, min, max);
}

fn run_tests(controllers: Vec<(i32, i32)>, min: i32, max: i32) -> (i64, i64) {
    let mut result: (i64, i64) = (-1, 2_i64.pow(32));
    println!("Total controllers: {}", controllers.len());
    for i in min..max {
        let cost: i64 = calculate_cost(&controllers, i);
        if cost < result.1 {result = (i as i64, cost)}
        println!("Finished test {}", i);
    }
    return result;
}

fn calculate_cost(controllers: &Vec<(i32, i32)>, distance: i32) -> i64 {
    let mut result: i64 = 0;
    for (index, controller) in controllers.iter().enumerate() {
        for i in 1..((controller.0 - distance).abs()+1) {
            result += (i as i64) * (controller.1 as i64);
        }
    }
    return result;
}
