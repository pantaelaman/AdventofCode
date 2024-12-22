#![feature(isqrt)]
fn main() {
    let input = std::env::args()
        .skip(1)
        .next()
        .unwrap()
        .parse::<i32>()
        .unwrap();
    println!("Part 1: {}", part1(input));
    println!("Part 2: https://oeis.org/A141481 & https://oeis.org/A141481/b141481.txt");
}

fn part1(input: i32) -> i32 {
    if input == 1 {
        return 0;
    };
    let layer = ((f32::sqrt(input as f32).ceil() - 1.) / 2.).ceil() as i32;
    let n = input - (((2 * layer - 1).pow(2)) + 1);
    let length = layer * 2;
    let lower = length * (n / length);
    let upper = lower + length - 1;
    let mid = (lower + upper - 1) / 2;
    println!("{}[{}]{}:{}-{}-{}", layer, n, length, lower, mid, upper);
    layer + (n - mid).abs()
}
