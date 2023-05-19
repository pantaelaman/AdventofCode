use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};
use std::ops::{Deref, DerefMut};

type Input = File;

const REQUIRED_KEYS: [&[u8]; 7] = [
    "byr".as_bytes(),
    "iyr".as_bytes(),
    "eyr".as_bytes(),
    "hgt".as_bytes(),
    "hcl".as_bytes(),
    "ecl".as_bytes(),
    "pid".as_bytes(),
];

struct Passport(HashMap<[u8; 3], String>);

impl std::fmt::Debug for Passport {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Passport [")?;
        for key in self.keys() {
            write!(f, "{}, ", std::str::from_utf8(key).unwrap())?;
        }
        write!(f, "]")
    }
}

impl Passport {
    fn parse(input: &[u8]) -> Self {
        let mut passport = Passport(HashMap::new());

        for field in std::str::from_utf8(input)
            .unwrap()
            .split(|chr: char| chr.is_whitespace())
        {
            let (key, value) = field.split(':').collect_tuple().unwrap();
            let mut key = key.bytes();
            let key = [
                key.next().unwrap(),
                key.next().unwrap(),
                key.exactly_one().unwrap(),
            ];

            passport.insert(key, value.to_string());
        }

        passport
    }

    fn is_valid_p1(&self) -> bool {
        REQUIRED_KEYS.iter().all(|key| self.contains_key(*key))
    }

    fn is_valid_p2(&self) -> bool {
        REQUIRED_KEYS.iter().all(|key| self.contains_key(*key))
            && self
                .iter()
                .all(|(key, val)| match std::str::from_utf8(key).unwrap() {
                    "byr" => val
                        .parse::<usize>()
                        .map(|year| year >= 1920 && year <= 2002)
                        .unwrap_or(false),
                    "iyr" => val
                        .parse::<usize>()
                        .map(|year| year >= 2010 && year <= 2020)
                        .unwrap_or(false),
                    "eyr" => val
                        .parse::<usize>()
                        .map(|year| year >= 2020 && year <= 2030)
                        .unwrap_or(false),
                    "hgt" => {
                        static RE: Lazy<Regex> =
                            Lazy::new(|| Regex::new(r"^(\d+)(cm|in)$").unwrap());
                        RE.captures(val)
                            .map(|captures| -> Option<bool> {
                                let num = captures.get(1)?.as_str().parse::<usize>().ok()?;
                                match captures.get(2).unwrap().as_str() {
                                    "cm" => Some(num >= 150 && num <= 193),
                                    "in" => Some(num >= 59 && num <= 76),
                                    _ => unreachable!(),
                                }
                            })
                            .flatten()
                            .unwrap_or(false)
                    }
                    "hcl" => {
                        static RE: Lazy<Regex> =
                            Lazy::new(|| Regex::new(r"^#[0-9a-f]{6}$").unwrap());
                        RE.is_match(val)
                    }
                    "ecl" => match val.as_str() {
                        "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true,
                        _ => false,
                    },
                    "pid" => {
                        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\d{9}$").unwrap());
                        RE.is_match(val)
                    }
                    _ => true,
                })
    }
}

impl Deref for Passport {
    type Target = HashMap<[u8; 3], String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Passport {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn main() {
    let mut input: Input = File::open("input.txt").unwrap();

    println!("Part 1: {}", part1(&mut input));
    println!("Part 2: {}", part2(&mut input));
}

fn part1(input: &mut Input) -> usize {
    input.rewind().unwrap();

    let reader = BufReader::new(input);

    check_passports(Passport::is_valid_p1, reader)
}

fn part2(input: &mut Input) -> usize {
    input.rewind().unwrap();

    let reader = BufReader::new(input);

    check_passports(Passport::is_valid_p2, reader)
}

fn check_passports<R: Read>(validator: fn(&Passport) -> bool, reader: BufReader<R>) -> usize {
    reader
        .split(b'\n')
        .coalesce(|prev, cur| {
            if (match cur {
                Ok(ref v) => v,
                Err(_) => return Err((prev, cur)),
            })
            .is_empty()
                || (match prev {
                    Ok(ref v) => v,
                    Err(_) => return Err((prev, cur)),
                })
                .is_empty()
            {
                Err((prev, cur))
            } else {
                match prev {
                    Ok(mut p) => match cur {
                        Ok(c) => {
                            p.push(b'\n');
                            p.extend(c);
                            Ok(Ok(p))
                        }
                        _ => Err((Ok(p), cur)),
                    },
                    _ => Err((prev, cur)),
                }
            }
        })
        .filter(|line| match line {
            Ok(v) => !v.is_empty(),
            _ => false,
        })
        .map(|line| line.map(|v| Passport::parse(&v)))
        .filter(|passport| passport.as_ref().is_ok_and(|p| validator(p)))
        .count()
}
