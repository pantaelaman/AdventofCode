use itertools::Itertools;

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn part1(input: &str) -> usize {
    input
        .split("\n\n")
        .map(|group| {
            group
                .split("\n")
                .flat_map(|questionnaire| questionnaire.chars())
                .unique()
                .count()
        })
        .sum()
}

fn part2(input: &str) -> usize {
    input
        .split("\n\n")
        .map(|group| {
            let mut questionnaires = group
                .split("\n")
                .filter(|questionnaire| !questionnaire.is_empty());
            let keys = questionnaires.next().unwrap();
            let result = keys.chars().fold(keys.len(), |acc, key| {
                if questionnaires.clone().all(|q| q.contains(key)) {
                    acc
                } else {
                    acc - 1
                }
            });
            result
        })
        .sum()
}
