use std::fs::File;
use std::io::{BufRead,BufReader};
use std::cmp::min;

type Package = (i32,i32,i32);

fn main() {
    println!("Test: {}", process_input("Day2test.txt"));
    println!("Result: {}", process_input("Day2input.txt"));
}

fn process_input(file: &str) -> i64 {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);
    let mut materials: Vec<i32> = Vec::new();

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        materials.push(calculate_package_ribbon(parse_package(line.as_str())));
    }

    materials.into_iter().fold(0_i64, |acc,el| {
        acc + el as i64
    })
}

fn parse_package(string: &str) -> Package {
    let str_dims = string.split('x').take(3).collect::<Vec<&str>>();
    let result: Package = (str_dims[0].parse().expect("Bad number"),str_dims[1].parse().expect("Bad number"),str_dims[2].parse().expect("Bad number"));
    result
}

fn calculate_package(package: Package) -> i32 {
    let s1 = package.0 * package.1;
    let s2 = package.1 * package.2;
    let s3 = package.2 * package.0;
    let extra = min(s1,min(s2,s3));
    (2*(s1+s2+s3))+extra
}

fn calculate_package_ribbon(package: Package) -> i32 {
    let bow = package.0 * package.1 * package.2;
    let p1 = (package.0 + package.1) * 2;
    let p2 = (package.1 + package.2) * 2;
    let p3 = (package.2 + package.1) * 2;
    let ribbon = min(p1,min(p2,p3));
    ribbon+bow
}

