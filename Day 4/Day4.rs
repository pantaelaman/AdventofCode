use std::fs::File;
use std::io::{self, BufRead, BufReader};

type Coordinate = (usize, usize);

#[derive (Clone, Copy, Debug)]
struct BoardState {
    state: [[bool; 5]; 5],
}

impl BoardState {
    pub fn new() -> BoardState {
        BoardState{state: [[false; 5]; 5]}
    }
    pub fn set_at(&mut self, value: bool, coords: Coordinate) {
        self.state[coords.0][coords.1] = value;
    }
    pub fn get_at(&self, coords: Coordinate) -> bool {
        self.state[coords.0][coords.1]
    }
    pub fn reorient(&self) -> BoardState {
        let mut state = BoardState::new();
        for i in 0..5 {
            for j in 0..5 {
                state.set_at(self.get_at((i as usize, j as usize)), (j as usize, i as usize));
            }
        }
        return state;
    }
    pub fn row_win(&self) -> bool {
        for i in 0..5 {
            if !self.state[i].to_vec().contains(&false) {return true}
        }
        return false;
    }
}

#[derive (Clone, Copy, Debug)]
struct Board {
    board: [[i32; 5]; 5],
    state: BoardState,
    active: bool,
}

impl Board {
    pub fn new() -> Board {
        Board{board: [[0; 5]; 5], state: BoardState::new(), active: true}
    }
    pub fn set_at(&mut self, value: i32, coords: Coordinate) {
        self.board[coords.0][coords.1] = value;
    }
    pub fn scratch_number(&mut self, num: i32) {
        for (i,column) in self.board.into_iter().enumerate() {
            for (j,cell) in column.into_iter().enumerate() {
                if *cell == num {
                    self.state.set_at(true, (i, j));
                }
            }
        }
    }
    pub fn check_for_win(&self) -> bool {
        if self.state.row_win() || self.state.reorient().row_win() {return true}
        return false
    }
    pub fn calculate_raw_score(&self) -> i32 {
        let mut buffer: i32 = 0;
        for i in 0..5 {
            for j in 0..5 {
                if !self.state.get_at((i, j)) {
                    buffer += self.board[i][j];
                }
            }
        }
        return buffer;
    }
}

fn main() {
    let (mut boards, draws) = process_input("Day4Input.txt");
    let mut score_raw: i32 = -1;
    let mut score: i32 = -1;
    let mut active_count: i32 = boards.len() as i32;
    'base: for (_,draw) in draws.iter().enumerate() {
        for (index,board) in boards.iter_mut().enumerate() {
            if !board.active {continue}
            board.scratch_number(*draw);
            if board.check_for_win() {
                if active_count == 1 {
                    score_raw = board.calculate_raw_score();
                    score = score_raw * *draw;
                    println!("Winning board #: {} with appearance: {:#?}", index, board);
                    println!("{}", *draw);
                    break 'base;
                } else {
                    board.active = false;
                    active_count -= 1;
                }
            }
        }
    }
    println!("Raw: {}", score_raw);
    println!("{}", score);
}

fn process_input(file: &str) -> (Vec<Board>, Vec<i32>) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);
    let mut boards: Vec<Board> = Vec::new();
    let mut draws: Vec<i32> = Vec::new();

    let mut reading = false;
    let mut board_col = 0;
    let mut temp_board = Board::new();

    for (index,line) in reader.lines().enumerate() {
        if index == 0 {
            draws = parse_sequence(line.expect("Could not read line 0"));
            continue;
        }
        let line = line.expect(format!("Could not read line {}", index).as_str());
        if line == "" {reading = true; continue}
        if reading {
            let mut sub = 0;
            for (row,digit) in line.split(" ").enumerate() {
                if digit == "" {sub+=1;continue}
                let val = digit.parse::<i32>().expect(format!("Could not parse digit {} on line {}", row-sub, index).as_str());
                println!("{}", val);
                temp_board.set_at(val, (board_col, row-sub));
            }
            board_col += 1;
        }
        
        if board_col > 4 {
            println!("Board: {:#?}", temp_board);
            boards.push(temp_board.clone());
            temp_board = Board::new();
            board_col = 0;
        }
    }
    
    return (boards,draws);
}

fn parse_sequence(seq: String) -> Vec<i32> {
    let mut sequence: Vec<i32> = Vec::new();
    
    for (i,item) in seq.split(",").enumerate() {
        sequence.push(item.parse::<i32>().expect(format!("Could not parse draw {}", i).as_str()));
    }

    return sequence;
}
