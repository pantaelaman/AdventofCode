use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Iterator;

#[derive (Clone, Debug)]
struct Program {
    vec: Vec<i32>,
    ptr: usize,
}

impl Program {
    pub fn new(vec: Vec<i32>) -> Program {
        Program{vec, ptr: 0}
    }

    pub fn next(&mut self) -> i32 {
        self.ptr += 1;
        if self.ptr >= self.vec.len() {
            self.ptr = 0;
            return *self.vec.last().unwrap();
        }
        self.vec[self.ptr - 1]
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
    let program = Program::new(vec_prog);
    println!("Final: {:?}", try_values(&program, 19690720));
}

fn try_values(program: &Program, look_for: i32) -> i32 {
    for n in 0..100 {
        for v in 0..100 {
            let mut current = program.clone();
            current.write(1, n);
            current.write(2, v);
            current.run();
            if current.read(0) == look_for {
                return (100 * n) + v;
            }
        }
    }
    return -1;
}
