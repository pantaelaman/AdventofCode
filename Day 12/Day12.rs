use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::{HashMap, HashSet};

fn main() {
    process_input("Day12Test0.txt");
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

    optimize(&mut caves);

    println!("{:?}", caves);
}

fn optimize(graph: &mut HashMap<String, HashSet<String>>) {
    let mut remove: HashSet<String> = HashSet::new();
    for (key,value) in graph.clone().iter() {
        if *key == "start".to_string() || *key == "end".to_string() || *key == key.to_ascii_uppercase() {continue}
        if value.iter().all(|el| *el == el.to_ascii_lowercase()) {
            println!("Removing {}", key);
            remove.insert(key.clone());
        }
    }
    for value in graph.values_mut() {
        for r in remove.iter() {
            if value.contains(r) {value.remove(r);}
        }
    }
    for r in remove.drain() {
        graph.remove(&r);
    }
}

fn dfs(graph: HashMap<String, HashSet<String>>, node: String, visited: HashSet<String>) {
    
}
