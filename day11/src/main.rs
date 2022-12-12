use std::fs::File;
use std::io::{BufRead, BufReader};

fn add(old: i128, vleft: &Value, vright: &Value) -> i128 {
    vleft.get_value(old) + vright.get_value(old)
}

fn sub(old: i128, vleft: &Value, vright: &Value) -> i128 {
    vleft.get_value(old) - vright.get_value(old)
}

fn mul(old: i128, vleft: &Value, vright: &Value) -> i128 {
    println!("Values: {:?}, {:?}", vleft, vright);
    vleft.get_value(old) * vright.get_value(old)
}

fn div(old: i128, vleft: &Value, vright: &Value) -> i128 {
    vleft.get_value(old) / vright.get_value(old)
}

enum Value {
    Old,
    Val(i128),
}

impl Value {
    fn get_value(&self, old: i128) -> i128 {
        match self {
            Self::Old => old,
            Self::Val(i) => *i,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Self::Old => write!(f, "Old"),
            Self::Val(i) => write!(f, "{}", i),
        }
    }
}

struct Monkey {
    items: Vec<i128>,
    vleft: Value,
    vright: Value,
    test_case: i128,
    test_true: usize,
    test_false: usize,
    inspection_counter: i128,
    operation: &'static dyn Fn(i128, &Value, &Value) -> i128, // (old, vleft, vright)
}

impl Monkey {
    fn inspect_items(
        &mut self,
        extras: &mut Vec<i128>,
        num_monkeys: usize,
        modulus: i128,
    ) -> Vec<Vec<i128>> {
        let mut moves = vec![Vec::new(); num_monkeys as usize];
        for item in self.items.drain(0..).chain(extras.drain(0..)) {
            let v = (self.operation)(item, &self.vleft, &self.vright) % modulus;
            if v % self.test_case == 0 {
                moves[self.test_true].push(v)
            } else {
                moves[self.test_false].push(v)
            }
            self.inspection_counter += 1;
        }
        moves
    }
}

impl std::fmt::Debug for Monkey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        f.debug_struct("Monkey")
            .field("items", &self.items)
            .field("vleft", &self.vleft)
            .field("vright", &self.vright)
            .field("test_case", &self.test_case)
            .field("test_true", &self.test_true)
            .field("test_false", &self.test_false)
            .finish()
    }
}

fn run_simulation(mut monkeys: Vec<Monkey>) -> (i128, i128) {
    let num_monkeys = monkeys.len();
    let modulus = monkeys.iter().map(|m| m.test_case).product();
    let mut extras = vec![Vec::new(); num_monkeys];
    let mut p1 = 0;
    for i in 0..10000 {
        // 20 rounds
        for (j, monkey) in &mut monkeys.iter_mut().enumerate() {
            for (k, mut e) in monkey
                .inspect_items(&mut extras[j], num_monkeys, modulus)
                .drain(0..)
                .enumerate()
            {
                extras[k].append(&mut e);
            }
        }

        println!("Finished {i}");

        if i + 1 == 20 {
            let mut top = (0, 0);
            for monkey in &monkeys {
                if monkey.inspection_counter > top.0 {
                    top.1 = top.0;
                    top.0 = monkey.inspection_counter;
                } else if monkey.inspection_counter > top.1 {
                    top.1 = monkey.inspection_counter;
                }
            }
            p1 = top.0 * top.1;
        }
    }
    let mut top = (0, 0);
    for monkey in &monkeys {
        if monkey.inspection_counter > top.0 {
            top.1 = top.0;
            top.0 = monkey.inspection_counter;
        } else if monkey.inspection_counter > top.1 {
            top.1 = monkey.inspection_counter;
        }
    }
    let p2 = top.0 * top.1;
    return (p1, p2);
}

fn read_monkeys(filename: &str) -> Vec<Monkey> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let mut monkeys = Vec::new();
    while let Some(_) = lines.next() {
        let items = lines
            .next()
            .unwrap()
            .unwrap()
            .split(": ")
            .skip(1)
            .next()
            .unwrap()
            .split(", ")
            .map(|e| i128::from_str_radix(e, 10).unwrap())
            .collect::<Vec<i128>>();

        let operation_parts = lines
            .next()
            .unwrap()
            .unwrap()
            .split("= ")
            .skip(1)
            .next()
            .unwrap()
            .split(" ")
            .map(|e| e.to_string())
            .collect::<Vec<String>>();

        println!("{:?}", operation_parts);
        let vleft = match operation_parts[0].as_str() {
            "old" => Value::Old,
            s => Value::Val(i128::from_str_radix(s, 10).unwrap()),
        };
        let vright = match operation_parts[2].as_str() {
            "old" => Value::Old,
            s => Value::Val(i128::from_str_radix(s, 10).unwrap()),
        };

        let operation: &'static dyn Fn(i128, &Value, &Value) -> i128 =
            match operation_parts[1].as_str() {
                "+" => &add,
                "-" => &sub,
                "*" => &mul,
                "/" => &div,
                _ => unimplemented!(),
            };

        let test_case = i128::from_str_radix(
            lines.next().unwrap().unwrap().split(" ").last().unwrap(),
            10,
        )
        .unwrap();

        let test_true = usize::from_str_radix(
            lines.next().unwrap().unwrap().split(" ").last().unwrap(),
            10,
        )
        .unwrap();

        let test_false = usize::from_str_radix(
            lines.next().unwrap().unwrap().split(" ").last().unwrap(),
            10,
        )
        .unwrap();

        monkeys.push(Monkey {
            items,
            vleft,
            vright,
            inspection_counter: 0,
            operation,
            test_case,
            test_true,
            test_false,
        });

        lines.next();
    }

    monkeys
}

fn main() {
    let monkeys = read_monkeys("input.txt");
    println!("{:#?}", monkeys);
    let level = run_simulation(monkeys);
    println!("Part 1: {}", level.0);
    println!("Part 2: {}", level.1);
}
