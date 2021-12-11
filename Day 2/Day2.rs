use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

struct Program {
    vec: Vec<i32>,
    pos: usize,
}

impl Program {
    pub fn new(vec: Vec<i32>) -> Program {
        Program{vec, pos: 0}
    }

    pub fn next(&mut self) -> i32 {
        self.pos += 1;
        if self.pos >= self.vec.len() {
            self.pos = 0;
            return *self.vec.last().unwrap();
        }
        self.vec[self.pos - 1]
    }

    pub fn read(&self, addr: i32) -> i32 {
        self.vec[addr as usize]
    }

    pub fn write(&mut self, addr: i32, val: i32) {
        self.vec[addr as usize] = val
    }

    fn run(&mut self) {
        match self.next() {
            1 => {
                let p1 = self.next();
                let p2 = self.next();
                let o1 = self.next();
                self.write(o1,self.read(p1) + self.read(p2));
            },
            2 => {
                let p1 = self.next();
                let p2 = self.next();
                let o1 = self.next();
                self.write(o1,self.read(p1) * self.read(p2));
            },
            99 => (return),
            _ => (panic!("Not a viable operator!!")),
        }
        self.run();
    }
}



fn main() {
    process_input("Day2Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);

    let vec_prog = reader.lines().nth(0).unwrap().expect("Could not read line 0").split(",").map(|el| el.parse::<i32>().expect("Bad format in program")).collect::<Vec<i32>>();
    let mut program = Program::new(vec_prog);
    program.run();
    println!("Final: {:?}", program.vec);
}
