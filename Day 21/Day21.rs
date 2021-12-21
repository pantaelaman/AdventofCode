use std::fs::File;
use std::io::{BufRead,BufReader};

struct Player {
    position: i32,
    score: i32,
}

impl Player {
    fn new(position: i32) -> Player {
        Player{position, score: 0}
    }

    fn play(&mut self, step: i32) -> bool {
        self.position = (self.position + step) % 10;
        self.score += if self.position == 0 {10} else {self.position};
        self.score >= 1000
    }
}

struct DDie {
    position: i32,
    count: i32
}

impl DDie {
    fn new() -> DDie {
        DDie{position: 0, count: 0}
    }

    fn roll(&mut self) -> i32 {
        let result = if self.position == 99 {100} else {self.position + 1};
        self.position = (self.position + 1) % 100;
        self.count += 1;
        result
    }
}

struct QDie {
    position: i32,
}

impl QDie {
    fn new(position: i32) -> QDie {
        QDie{position}
    }

    fn play(&self, p1: &mut Player, p2: &mut Player, data: &mut QData) -> (i128,i128) {
        data.count += 1;
        data.accumulator += self.position;
        if data.count == 2 {
            let cur_acc = data.accumulator;
            data.accumulator = 0;
            data.count = 0;
            if data.turn == 0 {
                data.turn = 1;
                if p1.play(cur_acc + 3) {return (1,0)}
            } else {
                data.turn = 0;
                if p2.play(cur_acc + 3) {return (0,1)}
            }
        }
        let strand0 = QDie::new(0).play(p1,p2,data);
        let strand1 = QDie::new(1).play(p1,p2,data);
        let strand2 = QDie::new(2).play(p1,p2,data);
        return (strand0.0 + strand1.0 + strand2.0, strand0.1 + strand1.1 + strand2.1);
    }
}

struct QData {
    accumulator: i32,
    count: i32,
    turn: i32,
}

impl QData {
    fn new() -> QData {
        QData{accumulator: 0, count: -1, turn: 0}
    }
}

fn main() {
    process_input("Day21Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let lines = BufReader::new(file).lines().scan(
        0,
        |acc,el| Some(el.expect(format!("Could not read line {}", acc).as_str()))
    ).collect::<Vec<String>>();

    let mut player1 = Player::new(lines[0].chars().skip(28).collect::<String>().parse().expect("Could not read starting position of player 1"));
    let mut player2 = Player::new(lines[1].chars().skip(28).collect::<String>().parse().expect("Could not read starting position of player 2"));

    let die0 = QDie::new(0).play(&mut player1, &mut player2, &mut QData::new());
    let die1 = QDie::new(1).play(&mut player1, &mut player2, &mut QData::new());
    let die2 = QDie::new(2).play(&mut player1, &mut player2, &mut QData::new());

    println!("Results: {:#?}", (die0.0 + die1.0 + die2.0, die0.1 + die1.1 + die2.1));
}
