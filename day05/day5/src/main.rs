use fancy_regex::Regex;
use lazy_static::lazy_static;
use indicatif::ProgressBar;

lazy_static! {
    static ref DOUBLES: Regex = Regex::new(r"^.*(.)\1.*").unwrap();
    static ref VOWELS: Regex = Regex::new(r"^.*[aeiou].*[aeiou].*[aeiou].*$").unwrap();
    static ref NOQUIRKS: Regex = Regex::new(r"^(?:(?!(ab|cd|pq|xy)).)+$").unwrap();
    static ref PART2: Regex = Regex::new(r"^(?=.*(.)(.).*\1\2.*)(?=.*(.).\3).*$").unwrap();
}

fn main() -> Result<(), std::io::Error> {
    let spinner: ProgressBar = ProgressBar::new_spinner();
    let (result1, result2) = read_lines(&spinner)?;
    println!("Part 1: {}", result1);
    println!("Part 2: {}", result2);
    Ok(())
}

fn read_lines(spinner: &ProgressBar) -> Result<(i32, i32), std::io::Error> {
    let mut total1 = 0;
    let mut total2 = 0;
    for line in std::io::stdin().lines() {
        spinner.inc(1);
        let line = line?;
        spinner.set_message(line.clone());
        if judge_string_1(&line) { total1 += 1 }
        if judge_string_2(&line) { total2 += 1 }
    }
    Ok((total1, total2))
}

fn judge_string_1(string: &String) -> bool {
    return 
        DOUBLES.is_match(string.as_str()).unwrap() &&
        VOWELS.is_match(string.as_str()).unwrap() &&
        NOQUIRKS.is_match(string.as_str()).unwrap();
}

fn judge_string_2(string: &String) -> bool {
    return PART2.is_match(string.as_str()).unwrap();
}
