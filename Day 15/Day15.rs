use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::{BinaryHeap,HashMap};
use std::cmp::Reverse;

struct HeapHolder<T, F: Ord> {
    priority: F,
    held: T,
}

impl<T,F: Ord> HeapHolder<T,F> {
    fn new(held: T, priority: F) -> Self {
        HeapHolder{priority,held}
    }
}

impl<T,F: Ord> HeapHolder<T,F> {
    fn get(&self) -> &T {
        &self.held
    }
    fn get_mut(&mut self) -> &mut T {
        &mut self.held
    }
    fn get_priority(&self) -> &F {
        &self.priority
    }
}

impl<T,F: Ord> PartialEq for HeapHolder<T,F> {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl<T,F: Ord> Eq for HeapHolder<T,F> {}

impl<T,F: Ord> PartialOrd for HeapHolder<T,F> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.priority.cmp(&other.priority))
    }
}

impl<T,F: Ord> Ord for HeapHolder<T,F> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

fn main() {
    process_input("Day15Input.txt");
}

fn process_input(filepath: &str) {
    let file = File::open(filepath).expect("Bad path");
    let mut map: Vec<Vec<(i32,(usize,usize),Vec<(usize,usize)>)>> = Vec::new();
    let reader = BufReader::new(file);
    let counter = BufReader::new(File::open(filepath).unwrap());
    let linecount = counter.lines().count();

    for (i,line) in reader.lines().enumerate() {
        let row_neighbors: Vec<usize>;
        if i == 0 {
            row_neighbors = vec![i+1];
        } else if i == linecount-1 {
            row_neighbors = vec![i-1];
        } else {
            row_neighbors = vec![i-1,i+1];
        }
        let line = line.expect(format!("Could not read line {}", i).as_str());
        let charcount = line.len();
        let mut row: Vec<(i32,(usize,usize),Vec<(usize,usize)>)> = Vec::new();
        for (j,chr) in line.chars().enumerate() {
            let col_neighbors: Vec<usize>;
            if j == 0 {
                col_neighbors = vec![j+1];
            } else if j == charcount-1 {
                col_neighbors = vec![j-1];
            } else {
                col_neighbors = vec![j-1,j+1];
            }
            let mut neighbors: Vec<(usize,usize)> = Vec::new();
            for k in &row_neighbors {
                neighbors.push((*k,j));
            }
            for k in &col_neighbors {
                neighbors.push((i,*k));
            }
            let main: i32 = chr.to_string().parse().unwrap();
            let index = (i,j);
            row.push((main,index,neighbors));
        }
        map.push(row);
    }
    let (cost,path) = dijkstra(&map);
    println!("{:?}\nCost: {}", path,cost);
}

fn dijkstra(map: &Vec<Vec<(i32,(usize,usize),Vec<(usize,usize)>)>>) -> (i32,Vec<(usize,usize)>) {
    let mut queue: BinaryHeap<HeapHolder<&(i32,(usize,usize),Vec<(usize,usize)>),Reverse<i32>>> = BinaryHeap::new();
    queue.push(HeapHolder::new(&map[0][0], Reverse(0)));
    let mut cur_cost: HashMap<(usize,usize),i32> = HashMap::new();
    cur_cost.insert((0,0),0);
    let mut came_from: HashMap<(usize,usize),Option<(usize,usize)>> = HashMap::new();
    came_from.insert((0,0),None);
    let goal = (map.len()-1,map[0].len()-1);

    while !queue.is_empty() {
        let raw_current = queue.pop().unwrap();
        let current = raw_current.get();

        if current.1 == goal {break}

        for nextindex in &current.2 {
            let next = &map[nextindex.0][nextindex.1];
            let new_cost = cur_cost.get(&current.1).unwrap() + next.0;
            if match cur_cost.get(&next.1) {
                Some(val) => new_cost < *val,
                None => true,
            } {
                *cur_cost.entry(next.1).or_insert(0) = new_cost;
                queue.push(HeapHolder::new(next,Reverse(new_cost)));
            }
            came_from.insert(*nextindex, Some(current.1));
        }
    }

    println!("{:?}", came_from.get(&(0,0)));

    let mut path: Vec<(usize,usize)> = Vec::new();
    let mut active = goal;
    /* loop {
        path.push(active);
        match came_from.get(&active).unwrap() {
            Some(loc) => active = *loc,
            None => break,
        };
    } */

    match cur_cost.get(&goal) {
        Some(val) => (*val,path),
        None => (-1,path),
    }
}
