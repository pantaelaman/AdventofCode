use std::fs::File;
use std::io::{self,BufRead,BufReader};

enum Direction {
    FWD,
    ASC,
    DSC
}

struct Command {
    direction: Direction,
    power: i32,
}

struct Position {
    depth: i32,
    distance: i32,
}

fn main() {
    let commands = process_input("Day2Input.txt");
    let mut position: Position = Position{depth: 0, distance: 0};
    for (index, command) in commands.iter().enumerate() {
        run_command(command, &mut position);
    }
    println!("{}", position.depth * position.distance);
}

fn run_command(command: &Command, position: &mut Position) {
    match (*command).direction {
        Direction::FWD => position.distance += (*command).power,
        Direction::ASC => position.depth -= (*command).power,
        Direction::DSC => position.depth += (*command).power,
    }
}

fn process_input(file: &str) -> Vec<Command> {
    let file = File::open(file).expect("Bad path");
    let mut ret: Vec<Command> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line: String = line.expect(format!("Could not read line {}", index).as_str());
        let direction = process_direction(&line.as_str()[0..(line.len()-2)]).expect(format!("Could not parse direction at line {}", index).as_str());
        let power = String::from(line.chars().nth(line.len()-1).expect(format!("Could not read char at the end of line {}", index).as_str())).parse::<i32>().expect(format!("Could not parse power at line {}", index).as_str());
        let command = Command{direction, power};
        ret.push(command);
    }

    return ret;
}

fn process_direction(raw: &str) -> Result<Direction, &str> {
    match raw {
        "forward" => return Ok(Direction::FWD),
        "up" => return Ok(Direction::ASC),
        "down" => return Ok(Direction::DSC),
        _ => return Err("Could not parse command"),
    }
}
