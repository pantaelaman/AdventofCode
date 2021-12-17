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

    fn run_packet(&self) -> u64 {
        match self {
            Packet::Literal(_, value) => {
                // Literal value
                *value
            },
            Packet::Operator(_, ptype, packets) => {
                match ptype {
                    0 => {
                        // Sum
                        let mut result = 0_u64;
                        for packet in packets {
                            result += packet.run_packet();
                        }
                        result
                    },
                    1 => {
                        // Product
                        let mut result = packets[0].run_packet();
                        for packet in &packets[1..] {
                            result *= packet.run_packet();
                        }
                        result
                    },
                    2 => {
                        // Minimum
                        let mut result = u64::MAX;
                        for packet in packets {
                            let packet_result = packet.run_packet();
                            if packet_result < result {
                                result = packet_result;
                            }
                        }
                        result
                    },
                    3 => {
                        // Maximum
                        let mut result = 0_u64;
                        for packet in packets {
                            let packet_result = packet.run_packet();
                            if packet_result > result {
                                result = packet_result;
                            }
                        }
                        result
                    },
                    5 => {
                        // Greater Than
                        let result = if packets[0].run_packet() > packets[1].run_packet() {1} else {0};
                        result
                    },
                    6 => {
                        // Less Than
                        let result = if packets[0].run_packet() < packets[1].run_packet() {1} else {0};
                        result
                    },
                    7 => {
                        // Equals
                        let result = if packets[0].run_packet() == packets[1].run_packet() {1} else {0};
                        result
                    },
                    _ => panic!("Unexpected type"),
                }
            }
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

    let mut code: VecDeque<char> = raw.chars().fold(VecDeque::new(), |mut acc,chr| {
        acc.push_back(chr);
        acc
    });

    run_program(&mut code);
}

fn run_program(code: &mut VecDeque<char>) {
    let mut queue: Program = Program::new();
    fill_queue(code, &mut queue);
    // Get the main packet
    let packet = parse_packet(&mut queue);
    // Part 1
    println!("Version Count: {}", packet.count_versions());
    // Part 2
    println!("Result: {}", packet.run_packet());
}

fn parse_packet(queue: &mut Program) -> Packet {
    // Pop the first 6 bits and parse them to get the version and packet type
    let version = parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u8;
    let ptype = parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u8;
    if ptype == 4 {
        // Packet Type is a Literal value
        let mut result: u64 = 0b0000;
        let mut segments: Vec<u64> = Vec::new();
        loop {
            // Add segments until you see a segment that begins with a 0
            if queue.pop_front().unwrap() {
                segments.push(parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u64);
            } else {
                segments.push(parse_programmed(&[queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap(),queue.pop_front().unwrap()]) as u64);
                break;
            }
        }

        // Push the binary segments into a u64
        for (index,segment) in segments.iter().enumerate() {
            let actual = segment << index * 4;
            result = result | actual;
        }
        return Packet::Literal(version, result);
    } else {
        // Packet Type is an operator
        if queue.pop_front().unwrap() {
            // Length ID is 1, so the next 11 bits represent the number of subpackets which follow
            let mut to_process: Vec<bool> = Vec::new();
            // Get the next eleven numbers
            for _ in 0..11 {
                to_process.push(queue.pop_front().unwrap());
            }
            let expected = parse_programmed(&to_process);
            let mut packets: Vec<Packet> = Vec::new();
            for _ in 0..(expected) {
                packets.push(parse_packet(queue));
            }
            return Packet::Operator(version, ptype, packets);
        } else {
            // Length ID is 0, so the next 15 bits represent the length (in bits) of the total of the subpackets which follow
            let mut to_process: Vec<bool> = Vec::new();
            for _ in 0..15 {
                to_process.push(queue.pop_front().unwrap());
            }
            let expected = parse_programmed(&to_process);
            let orig_size = queue.len();
            let mut cur_size = queue.len();
            let mut packets: Vec<Packet> = Vec::new();
            // While the total number of bits that the packets have consumed is not equal to the expected amount, keep reading packets
            while orig_size - cur_size != expected {
                packets.push(parse_packet(queue));
                cur_size = queue.len();
            }
            return Packet::Operator(version, ptype, packets);
        }
    }
}

fn fill_queue(original: &mut VecDeque<char>, into: &mut Program) {
    // Drain a character array into a Program
    for chr in original {
        into.append(&mut parse_hexadecimal(*chr).into_iter().collect());
    }
}

fn parse_hexadecimal(hex: char) -> Vec<bool> {
    // Parse a hexadecimal character into a vector of bools
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
    // Parse an array of booleans into a binary number
    let size = programmed.len();
    let mut result: usize = 0b0000;
    for (i,item) in programmed.iter().enumerate() {
        let actual = match item {true => 1, false => 0} << (size - 1 - i);
        result = result | actual
    }
    result
}
