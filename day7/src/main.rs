use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, parents, children, weights) = part1(&mut reader);
    println!("Part 1: {}", p1);
}

fn part1<F: Read>(
    reader: &mut BufReader<F>,
) -> (
    String,
    HashMap<String, String>,
    HashMap<String, Vec<String>>,
    HashMap<String, usize>,
) {
    let mut parents: HashMap<String, String> = HashMap::new();
    let mut children: HashMap<String, Vec<String>> = HashMap::new();
    let mut weights: HashMap<String, usize> = HashMap::new();

    let re = Regex::new(r"(\w+) \((\d+)\)(?: -> (.*))?").unwrap();
    for line in reader.lines() {
        let line = line.unwrap();
        let captures = re.captures(&line).unwrap();
        let mut groups = captures.iter().inspect(|g| println!("{:?}", g));
        println!("");

        groups.next().unwrap();
        let name = groups.next().flatten().unwrap().as_str();
        println!("{}", name);
        let weight = groups
            .next()
            .flatten()
            .unwrap()
            .as_str()
            .parse::<usize>()
            .unwrap();
        weights.insert(name.to_string(), weight);
        let cur_children = match groups.next().flatten() {
            Some(g) => g.as_str().split(", ").map(|s| s.to_string()).collect_vec(),
            None => Vec::new(),
        };
        for child in cur_children.iter() {
            parents.insert(child.clone(), name.to_string());
        }
        children.insert(name.to_string(), cur_children);
    }

    println!("{:?}", children);
    println!("{:#?}", parents);

    (
        children
            .iter()
            .filter(|(k, _)| !parents.contains_key(*k))
            .exactly_one()
            .unwrap()
            .0
            .clone(),
        parents,
        children,
        weights,
    )
}

fn seek_imbalance(
    root: &String,
    children: &HashMap<String, Vec<String>>,
    weights: &HashMap<String, usize>,
) -> Result<usize, usize> {
    let root_children = children.get(root).unwrap();
    let num_children = root_children.len();
    if num_children == 0 || num_children == 1 {}
    Err(0)
}
