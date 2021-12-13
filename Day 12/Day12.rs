use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::{HashMap, HashSet};

fn main() {
    process_input("Day12Input.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    // HashMap<caveName, Vec<caveNames which are connected>>
    let mut caves: HashMap<String, HashSet<String>> = HashMap::new();
    let reader = BufReader::new(file);

    for (index,line) in reader.lines().enumerate() {
        let line = line.expect(format!("Could not read line {}", index).as_str());
        let parts: Vec<&str> = line.splitn(2,"-").collect();
        
        match caves.get_mut(&*parts[0]) {
            Some(value) => {value.insert(parts[1].to_string());},
            None => {caves.insert(parts[0].to_string(), vec![parts[1].to_string()].into_iter().collect::<HashSet<String>>());},
        };
        match caves.get_mut(&*parts[1]) {
            Some(value) => {value.insert(parts[0].to_string());},
            None => {caves.insert(parts[1].to_string(), vec![parts[0].to_string()].into_iter().collect::<HashSet<String>>());},
        };
    }

    println!("{}", dfs(&caves, &"start".to_string(), HashSet::new()));
}

fn dfs(graph: &HashMap<String, HashSet<String>>, node: &String, mut visited: HashSet<String>) -> i32 {
    if *node == "end".to_string() {return 1;}
    if visited.contains(node) {
        return 0;
    }
    if *node == node.to_ascii_lowercase() {visited.insert(node.clone());}
    let mut acc = 0;
    for n in graph.get(node).unwrap() {
        acc += dfs(graph, n, visited.clone());
    }
    return acc;
}
