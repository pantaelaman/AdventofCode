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

    let mut gamestate: (bool, i8) = (false, -1); // 0 => Player1, 1 => Player2
    let mut die = DDie::new();
    let mut turnnumber = 0;
    loop {
        let turn = die.roll() + die.roll() + die.roll();
        gamestate = (player1.play(turn), 0);
        turnnumber += 1;
        if gamestate.0 {break}
        // println!("TURN {}; P1 :: Die: {}, Position: {}, Increase: {}, Score: {}", turnnumber, die.position, player1.position, turn, player1.score);
        let turn = die.roll() + die.roll() + die.roll();
        gamestate = (player2.play(turn), 1);
        turnnumber += 1;
        if gamestate.0 {break}
        // println!("TURN {}; P2 :: Die: {}, Position: {}, Increase: {}, Score: {}", turnnumber, die.position, player2.position, turn, player2.score);
    }
    
    if gamestate.1 == 0 {
        println!("Player 1 wins; result: {} * {} = {}", player2.score, die.count, player2.score * die.count);
    } else if gamestate.1 == 1 {
        println!("Player 2 wins; result: {} * {} = {}", player1.score, die.count, player1.score * die.count);
    } else {
        println!("Something went wrong.");
    }
}
