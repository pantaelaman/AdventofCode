use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};

fn main() {
    let file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    let mut reader = BufReader::new(file);
    let (p1, _, children, weights) = part1(&mut reader);
    println!("Part 1: {}", p1);
    println!("Part 2: {}", part2(&p1, &children, &weights));
}

fn part1<F: Read>(
    reader: &mut BufReader<F>,
) -> (
    String,
    HashMap<String, String>,
    HashMap<String, Vec<String>>,
    HashMap<String, i32>,
) {
    let mut parents: HashMap<String, String> = HashMap::new();
    let mut children: HashMap<String, Vec<String>> = HashMap::new();
    let mut weights: HashMap<String, i32> = HashMap::new();

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
            .parse::<i32>()
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

fn part2(
    root: &String,
    children: &HashMap<String, Vec<String>>,
    weights: &HashMap<String, i32>,
) -> i32 {
    let (name, delta) = seek_imbalance(root, children, weights, 0);
    println!("imbalanced: {}", name);
    delta
}

fn seek_imbalance<'a>(
    root: &'a String,
    children: &'a HashMap<String, Vec<String>>,
    weights: &'a HashMap<String, i32>,
    delta: i32,
) -> (&'a String, i32) {
    let root_children = children.get(root).unwrap();
    let children_weights = root_children
        .iter()
        .map(|c| weights.get(c).unwrap())
        .collect_vec();
    // let num_children = root_children.len();
    // let weight = weights.get(root).unwrap();
    let mut weight_classes = children_weights
        .iter()
        .sorted()
        .dedup_with_count()
        .sorted_by(|(a, _), (b, _)| b.cmp(a))
        .map(|(_, c)| *c);
    let target = weight_classes.next().unwrap();
    match weight_classes.next() {
        Some(w) => {
            let unbalanced = root_children
                .iter()
                .filter(|c| weights.get(*c).unwrap() == w)
                .exactly_one()
                .unwrap();
            seek_imbalance(unbalanced, children, weights, w - target)
        }
        None => (root, weights.get(root).unwrap() + delta),
    }
}
