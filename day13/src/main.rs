use serde::Deserialize;
use std::fs::read_to_string;

#[derive(Deserialize, Clone, Debug)]
struct Packet(Vec<Value>);

impl std::ops::Deref for Packet {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::cmp::PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        match validate_packet_pair((self, other)) {
            Validation::Unsure => true,
            _ => false,
        }
    }
}

impl std::cmp::Eq for Packet {}

impl std::cmp::PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match validate_packet_pair((self, other)) {
            Validation::Valid => Some(std::cmp::Ordering::Less),
            Validation::Unsure => Some(std::cmp::Ordering::Equal),
            Validation::Invalid => Some(std::cmp::Ordering::Greater),
        }
    }
}

impl std::cmp::Ord for Packet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
enum Value {
    Int(i32),
    Vec(Packet),
}

enum Validation {
    Valid,
    Invalid,
    Unsure,
}

fn validate_packet_pair(pair: (&Packet, &Packet)) -> Validation {
    let mut iter0 = pair.0.iter();
    let mut iter1 = pair.1.iter();
    while let Some(value0) = iter0.next() {
        if let Some(value1) = iter1.next() {
            match (value0, value1) {
                (Value::Int(i0), Value::Int(i1)) => {
                    if i0 < i1 {
                        return Validation::Valid;
                    } else if i0 > i1 {
                        return Validation::Invalid;
                    }
                }
                (Value::Int(i0), Value::Vec(v1)) => {
                    match validate_packet_pair((&Packet(vec![Value::Int(*i0)]), v1)) {
                        Validation::Unsure => {}
                        v => return v,
                    }
                }
                (Value::Vec(v0), Value::Int(i1)) => {
                    match validate_packet_pair((v0, &Packet(vec![Value::Int(*i1)]))) {
                        Validation::Unsure => {}
                        v => return v,
                    }
                }
                (Value::Vec(v0), Value::Vec(v1)) => match validate_packet_pair((v0, v1)) {
                    Validation::Unsure => {}
                    v => return v,
                },
            }
        } else {
            return Validation::Invalid;
        }
    }

    if let Some(_) = iter1.next() {
        return Validation::Valid;
    }

    Validation::Unsure
}

fn read_packet_pairs(filename: &str) -> Vec<(Packet, Packet)> {
    read_to_string(filename)
        .unwrap()
        .split("\n\n")
        .map(|p| {
            let mut split = p.split("\n");
            (
                serde_json::from_str::<Packet>(split.next().unwrap()).unwrap(),
                serde_json::from_str::<Packet>(split.next().unwrap()).unwrap(),
            )
        })
        .collect()
}

fn read_packets(filename: &str) -> Vec<Packet> {
    read_to_string(filename)
        .unwrap()
        .split("\n")
        .filter(|s| s != &"")
        .map(|p| serde_json::from_str::<Packet>(p).unwrap())
        .collect()
}

fn main() {
    const FILENAME: &'static str = "input.txt";
    let sum: usize = read_packet_pairs(FILENAME)
        .into_iter()
        .enumerate()
        .map(|p| match validate_packet_pair((&p.1 .0, &p.1 .1)) {
            Validation::Valid => p.0 + 1,
            Validation::Invalid => 0,
            Validation::Unsure => panic!("Unsure value"),
        })
        .sum();

    println!("Part 1: {}", sum);

    let distress_p1 = Packet(vec![Value::Vec(Packet(vec![Value::Int(2)]))]);
    let distress_p2 = Packet(vec![Value::Vec(Packet(vec![Value::Int(6)]))]);

    let mut packets = read_packets(FILENAME);
    packets.append(&mut vec![distress_p1.clone(), distress_p2.clone()]);
    packets.sort();
    let i1 = packets.iter().position(|p| p == &distress_p1).unwrap() + 1;
    let i2 = packets.iter().rposition(|p| p == &distress_p2).unwrap() + 1;

    println!("Part 2: ({i1}, {i2}); {}", i1 * i2);
}
