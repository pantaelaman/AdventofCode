use std::fs::File;
use std::io::{BufRead,BufReader};
use std::collections::{BinaryHeap,HashMap};
use std::cmp::Reverse;

struct Neighbours {
    position: (usize,usize),
    size: usize,
    index: usize,
}

impl Neighbours {
    const OFFSETS: [(i32,i32); 4] = [(1,0),(-1,0),(0,1),(0,-1)];

    fn new(position: (usize,usize), size: usize) -> Neighbours {
        Neighbours{position,size,index: 0}
    }
}

// thanks u/gosslot
impl std::iter::Iterator for Neighbours {
    type Item = (usize,usize);
    fn next(&mut self) -> Option<Self::Item> {
        while self.index < 4 {
            let offset = Self::OFFSETS[self.index];
            self.index += 1;
            let dx = self.position.0 as i32 + offset.0;
            let dy = self.position.1 as i32 + offset.1;
            if dx >= 0 && (dx as usize) < self.size && dy >= 0 && (dy as usize) < self.size {
                return Some((dx as usize, dy as usize));
            }
        }
        None
    }
}

fn main () {
    process_input("Day15Test.txt");
}

fn process_input(file: &str) {
    let file = File::open(file).expect("Bad path");
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().scan(0, |acc, el| {
        let result = Some(el.expect(format!("Could not read line {}", acc).as_str()));
        *acc += 1;
        result
    }).collect();

    let size = lines.len();
    let mut map = Vec::with_capacity(size);
    for (index,line) in lines.iter().enumerate() {
        map.push(line.chars().scan(0, |acc, el| {
            let result = Some(el.to_string().parse::<i32>().expect(format!("Could not read char {} on line {}",acc,index).as_str()));
            *acc += 1;
            result
        }).collect::<Vec<i32>>());
    }
    let expanded = expand_map(map.clone());
    let size = expanded.len();
    let cost = dijkstra(&map, (0,0), (size - 1, size - 1));
    println!("Cost: {}", cost);
}

fn expand_map(map: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    let size = map.len();
    let mut result = vec![vec![0; size * 5]; size * 5];
    for row_offset in 0..5 {
        for (r, row) in map.iter().enumerate() {
            for col_offset in 0..5 {
                for (c, &item) in row.iter().enumerate() {
                    let new = (item + col_offset + row_offset) % 10;
                    result[row_offset as usize * size + r][col_offset as usize * size + c] = new;
                }
            }
        }
    }
    result
}

fn dijkstra(map: &Vec<Vec<i32>>, start: (usize,usize), goal: (usize,usize)) -> i32 {
    let size = map.len();
    let mut costs: HashMap<(usize,usize),i32> = HashMap::new();
    let mut previous: HashMap<(usize,usize),Option<(usize,usize)>> = HashMap::new();
    costs.insert(start, 0);
    previous.insert(start, None);
    let mut queue: BinaryHeap<Reverse<(i32,(usize,usize))>> = BinaryHeap::new();
    queue.push(Reverse((0,start)));

    while let Some(Reverse((cost,pos))) = queue.pop() {
        let neighbours = Neighbours::new(pos,size);
        for neighbour in neighbours {
            let new_cost = cost + map[neighbour.1][neighbour.0];
            if costs.get(&neighbour).map_or(true, |old| new_cost < *old) {
                queue.push(Reverse((new_cost, neighbour)));
                costs.insert(neighbour, new_cost);
                previous.insert(neighbour, Some(pos));
            }
        }
    }

    *costs.get(&goal).unwrap_or(&-1)
}
