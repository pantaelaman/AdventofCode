use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::VecDeque;

type Program = VecDeque<bool>;
type Version = u8;
type PacketType = u8;

#[derive(Debug)]
enum Packet {
    Literal(Version, u64),
    Operator(Version, PacketType, Vec<Packet>),
}

impl Packet {
    fn count_versions(&self) -> u64 {
        match self {
            Packet::Literal(version, _) => *version as u64,
            Packet::Operator(version, _, packets) => {
                let mut others = 0_u64;
                for packet in packets {
                    others += packet.count_versions();
                }
                *version as u64 + others
            },
        }
    }
}

fn main() {
    process_input("Day16Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);
    let raw = reader.lines().nth(0).unwrap().expect("Could not read code");

    let mut coded: VecDeque<char> = raw.chars().fold(VecDeque::new(), |mut acc,chr| {
        acc.push_back(chr);
        acc
    });

    println!("{:?}", coded);

    run_program(&mut coded);
}

fn run_program(encoded: &mut VecDeque<char>) {
    let mut queue: Program = Program::new();
    fill_queue(encoded, &mut queue);
    let packet = parse_packet(&mut queue);
    println!("Version Count: {}", packet.count_versions());
}

fn parse_packet(queue: &mut Program) -> Packet {
    let version = parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u8;
    let ptype = parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u8;
    println!("Version: {}", version);
    println!("Type: {}", ptype);
    if ptype == 4 {
        let mut result: u64 = 0b0000;
        let mut segments: Vec<u64> = Vec::new();
        loop {
            if queue.pop_front().unwrap() {
                segments.push(parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u64);
            } else {
                segments.push(parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u64);
                break;
            }
        }
        let total = segments.len();
        for (index,segment) in segments.iter().enumerate() {
            let actual = segment << (total-index) * 4;
            result = result | actual;
        }
        return Packet::Literal(version, result);
    } else {
        if queue.pop_front().unwrap() {
            let mut to_process: Vec<bool> = Vec::new();
            for _ in 0..11 {
                to_process.push(queue.pop_front().unwrap());
            }
            let expected = parse_programmed(&to_process);
            println!("Count: {}", expected);
            let mut packets: Vec<Packet> = Vec::new();
            for _ in 0..(expected) {
                packets.push(parse_packet(queue));
            }
            return Packet::Operator(version, ptype, packets);
        } else {
            let mut to_process: Vec<bool> = Vec::new();
            for _ in 0..15 {
                to_process.push(queue.pop_front().unwrap());
            }
            let expected = parse_programmed(&to_process);
            println!("Bits: {}", expected);
            let orig_size = queue.len();
            let mut cur_size = queue.len();
            let mut packets: Vec<Packet> = Vec::new();
            while orig_size - cur_size != expected {
                packets.push(parse_packet(queue));
                cur_size = queue.len();
            }
            return Packet::Operator(version, ptype, packets);
        }
    }
}

fn fill_queue(original: &mut VecDeque<char>, into: &mut VecDeque<bool>) {
    for chr in original {
        into.append(&mut parse_hexadecimal(*chr).into_iter().collect());
    }
}

fn parse_hexadecimal(hex: char) -> Vec<bool> {
    match hex {
        '0' => vec![false, false, false, false],
        '1' => vec![false, false, false, true],
        '2' => vec![false, false, true, false],
        '3' => vec![false, false, true, true],
        '4' => vec![false, true, false, false],
        '5' => vec![false, true, false, true],
        '6' => vec![false, true, true, false],
        '7' => vec![false, true, true, true],
        '8' => vec![true, false, false, false],
        '9' => vec![true, false, false, true],
        'A' | 'a' => vec![true, false, true, false],
        'B' | 'b' => vec![true, false, true, true],
        'C' | 'c' => vec![true, true, false, false],
        'D' | 'd' => vec![true, true, false, true],
        'E' | 'e' => vec![true, true, true, false],
        'F' | 'f' => vec![true, true, true, true],
        _ => panic!("Unexpected char"),
    }
}

fn parse_programmed(programmed: &[bool]) -> usize {
    let size = programmed.len();
    let mut result: usize = 0b0000;
    for (i,item) in programmed.iter().enumerate() {
        let actual = match item {true => 1, false => 0} << (size - 1 - i);
        result = result | actual
    }
    result
}
