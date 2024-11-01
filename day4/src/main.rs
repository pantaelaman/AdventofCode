use std::{
  collections::HashMap,
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use regex::Regex;

#[derive(Debug)]
struct Command {
  date: Date,
  minute: u32,
  change: StateChange,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct Date {
  year: u32,
  month: u32,
  day: u32,
}

impl PartialOrd for Date {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(
      self
        .year
        .cmp(&other.year)
        .then(self.month.cmp(&other.month))
        .then(self.day.cmp(&other.day)),
    )
  }
}

impl Ord for Date {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self
      .year
      .cmp(&other.year)
      .then(self.month.cmp(&other.month))
      .then(self.day.cmp(&other.day))
  }
}

impl std::fmt::Display for Date {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}-{}-{}", self.year, self.month, self.day)
  }
}

#[derive(Debug)]
enum StateChange {
  FallAsleep,
  WakeUp,
}

type Schedule = HashMap<Date, Vec<(u32, u32)>>;

fn total_minutes_asleep(schedule: &Schedule) -> u32 {
  schedule
    .values()
    .map(|v| v.iter().map(|(l, r)| r - l).sum::<u32>())
    .sum()
}

fn maximum_minute(schedule: &Schedule) -> (usize, usize) {
  let mut histogram: [usize; 60] = [0; 60];
  for pair in schedule.values().map(|pairs| pairs.iter()).flatten() {
    for i in pair.0..pair.1 {
      histogram[i as usize] += 1;
    }
  }
  let max_idx = histogram.iter().position_max().unwrap();
  (max_idx, histogram[max_idx])
}

fn main() {
  let file =
    File::open(std::env::args().skip(1).next().expect("Missing input file"))
      .unwrap();

  let reader = BufReader::new(file);

  let date_regex =
    Regex::new(r"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.*)").unwrap();
  let shiftchange_regex = Regex::new(r"Guard #(\d+) begins shift").unwrap();

  let mut dates_to_guards: HashMap<Date, usize> = HashMap::new();
  let mut unassigned_commands: Vec<Command> = Vec::new();
  for line in reader.lines() {
    let line = line.unwrap();

    let caps = date_regex.captures(&line).expect("Malformed input");
    let mut date = Date {
      year: caps[1].parse().unwrap(),
      month: caps[2].parse().unwrap(),
      day: caps[3].parse().unwrap(),
    };
    if caps[4].parse::<u32>().unwrap() == 23 {
      date.day += 1; // please let me get away with this shit
    }

    if let Some(guard_caps) = shiftchange_regex.captures(&caps[6]) {
      let guard_num = guard_caps[1].parse().unwrap();
      dates_to_guards.insert(date, guard_num);
      if !dates_to_guards.contains_key(&date) {}
      continue;
    }

    let change = match &caps[6] {
      "wakes up" => StateChange::WakeUp,
      "falls asleep" => StateChange::FallAsleep,
      _ => panic!("Malformed input"),
    };

    unassigned_commands.push(Command {
      date,
      minute: caps[5].parse().unwrap(),
      change,
    });
  }

  println!("{:?}", dates_to_guards);
  let mut partial_guards: HashMap<usize, Vec<Command>> = HashMap::new();
  for command in unassigned_commands {
    let guard_num = dates_to_guards
      .get(&command.date)
      .expect(&format!("Missing date {}", command.date));
    if !partial_guards.contains_key(guard_num) {
      partial_guards.insert(*guard_num, Vec::new());
    }
    partial_guards.get_mut(guard_num).unwrap().push(command);
  }

  let mut guards: HashMap<usize, Schedule> = HashMap::new();
  for (guard_num, mut commands) in partial_guards {
    commands.sort_by(|c1, c2| c1.date.cmp(&c2.date));
    println!("Guard #{}", guard_num);

    let mut schedule: Schedule = Schedule::new();
    for (date, commands) in
      commands.into_iter().chunk_by(|c| c.date).into_iter()
    {
      println!("\tDate {}", date);
      let mut commands = commands.collect_vec();
      commands.sort_by(|c1, c2| c1.minute.cmp(&c2.minute));
      for command in commands.iter() {
        println!("\t\t{} - {:?}", command.minute, command.change);
      }
      schedule.insert(
        date,
        commands
          .into_iter()
          .chunks(2)
          .into_iter()
          .map(|mut pair| {
            (
              pair.next().unwrap().minute,
              pair.next().map(|v| v.minute).unwrap_or(60),
            )
          })
          .collect_vec(),
      );
    }
    guards.insert(guard_num, schedule);
  }
  for (guard_num, schedule) in guards.iter() {
    println!(
      "Guard #{} total minutes asleep: {}",
      guard_num,
      total_minutes_asleep(&schedule)
    )
  }
  let (target_num, _, target_schedule) = guards
    .iter()
    .map(|(guard_num, schedule)| {
      (guard_num, total_minutes_asleep(schedule), schedule)
    })
    .max_by(|(_, m1, _), (_, m2, _)| m1.cmp(m2))
    .unwrap();
  let target_minute = maximum_minute(target_schedule).0;
  println!(
    "Guard #{} most frequent minute: {}",
    target_num, target_minute,
  );
  println!("Part 1: {}", target_num * target_minute);

  let (target_num, (target_minute, _)) = guards
    .iter()
    .map(|(guard_num, schedule)| (guard_num, maximum_minute(schedule)))
    .max_by(|(_, (_, m1)), (_, (_, m2))| m1.cmp(m2))
    .unwrap();
  println!(
    "Guard #{} most frequent minute: {}",
    target_num, target_minute
  );
  println!("Part 2: {}", target_num * target_minute);
}
