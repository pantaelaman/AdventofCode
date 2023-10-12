fn main() {
    println!("Part 1: {}", part1(23));
}

fn part1(input: u64) -> u64 {
    let mut side_length: u64 = 1;
    let mut max_num: u64 = 1;
    while max_num < input {
        side_length += 2;
        max_num = side_length.pow(2);
    }
    let distance = max_num - input;
    let side = distance / side_length;
    let offset = distance % side_length;
    0
}
