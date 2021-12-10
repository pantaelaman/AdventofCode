use std::fs::File;
use std::io::{BufRead,BufReader};

fn main() {
    process_input("Day10Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let mut completions: Vec<Vec<char>> = Vec::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        println!("-- Line {} --", index);
        let chars = line.chars().collect::<Vec<char>>();
        let chars = chars.iter();
        let (result,expect) = process_chars(chars);
        println!("Result: {}", result);
        if result == 0 {
            completions.push(expect);
        }
    }

    let mut result = completions.iter().map(|el| el.iter().rev().fold(0,|sacc,sel| {
        (sacc * 5) + match sel {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => panic!("Unexpected char"),
        }
    })).collect::<Vec<i64>>();
    result.sort();

    let middle = result[result.len() / 2];

    println!("Possible: {:?}", result);
    println!("Final: {}", middle);
}

fn process_chars(chars: std::slice::Iter<char>) -> (i64,Vec<char>) {
    let mut expect: Vec<char> = Vec::new();
    for chr in chars {
        match chr {
            '(' => expect.push(')'),
            '[' => expect.push(']'),
            '{' => expect.push('}'),
            '<' => expect.push('>'),
            ')' => if expect.pop().unwrap() != ')' {return (3,expect)},
            ']' => if expect.pop().unwrap() != ']' {return (57,expect)},
            '}' => if expect.pop().unwrap() != '}' {return (1197,expect)},
            '>' => if expect.pop().unwrap() != '>' {return (25137,expect)},
            _ => panic!("Unexpected char"),
        }
    }
    return (0,expect);
}
