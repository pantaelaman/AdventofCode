use pathfinding::directed::dijkstra::dijkstra;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone)]
struct Node {
    elevation: usize,
    position: Point,
    neighbours: Vec<Point>,
}

fn read_grid(filename: &str) -> (Vec<Vec<Node>>, Point, Point) {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    let mut node_grid: Vec<Vec<Node>> = Vec::new();
    let mut starting_point = Point { x: 0, y: 0 };
    let mut target_point = Point { x: 0, y: 0 };
    for (y, line) in reader.lines().map(|l| l.unwrap()).enumerate() {
        node_grid.push(Vec::new());
        for (x, mut point) in line.chars().enumerate() {
            if point == 'S' {
                point = 'a';
                starting_point = Point { x, y };
            }

            if point == 'E' {
                point = 'z';
                target_point = Point { x, y };
            }

            let mut node = Node {
                elevation: point as usize - 0x61,
                position: Point { x, y },
                neighbours: Vec::new(),
            };

            let elevation_range;
            if node.elevation > 0 {
                elevation_range = (node.elevation - 1)..(node.elevation + 2);
            } else {
                elevation_range = node.elevation..(node.elevation + 2);
            }

            // check previous row
            if y > 0 {
                if elevation_range.contains(&node_grid[y - 1][x].elevation) {
                    node_grid[y - 1][x].neighbours.push(Point { x, y });
                    node.neighbours.push(Point { x, y: y - 1 });
                } else if node.elevation > 0 && node_grid[y - 1][x].elevation < node.elevation - 1 {
                    node.neighbours.push(Point { x, y: y - 1 });
                } else {
                    node_grid[y - 1][x].neighbours.push(Point { x, y });
                }
            }

            // check previous column
            if x > 0 {
                if elevation_range.contains(&node_grid[y][x - 1].elevation) {
                    node_grid[y][x - 1].neighbours.push(Point { x, y });
                    node.neighbours.push(Point { x: x - 1, y });
                } else if node.elevation > 0 && node_grid[y][x - 1].elevation < node.elevation - 1 {
                    node.neighbours.push(Point { x: x - 1, y });
                } else {
                    node_grid[y][x - 1].neighbours.push(Point { x, y });
                }
            }

            node_grid[y].push(node);
        }
    }

    (node_grid, starting_point, target_point)
}

fn main() {
    let (nodes, start, end) = read_grid("input.txt");

    match dijkstra(
        &&start,
        |p| nodes[p.y][p.x].neighbours.iter().map(|n| (n, 1)),
        |p| p == &&end,
    ) {
        Some(p) => println!("Part 1: {}", p.1),
        None => println!("Part 1: No path found."),
    }

    // Second Part
    // Only find paths for 'a' locations where they have valid neighbours
    // Otherwise we're wasting time
    let shortest = nodes
        .iter()
        .flatten()
        .filter(|n| n.elevation == 0 && !n.neighbours.is_empty())
        .map(|n| n.position)
        .filter_map(|p| {
            dijkstra(
                &&p,
                |p| nodes[p.y][p.x].neighbours.iter().map(|n| (n, 1)),
                |p| p == &&end,
            )
            .map(|p| p.1)
        })
        .min();

    match shortest {
        Some(s) => println!("Part 2: {}", s),
        None => println!("Part 2: No path found."),
    }
}
