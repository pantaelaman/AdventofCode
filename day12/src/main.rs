use std::io::stdin;

use itertools::Itertools;
use json::JsonValue;

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  let contents = lines.join("\n");

  let p = json::parse(&contents).unwrap();
  println!("Part 1: {}", sum_value(&p));
  println!("Part 2: {}", sum_avoiding_red(&p));
}

fn sum_value(val: &JsonValue) -> i64 {
  match val {
    JsonValue::Number(n) => n.as_fixed_point_i64(0).unwrap(),
    JsonValue::Array(v) => v.iter().map(sum_value).sum(),
    JsonValue::Object(o) => o.iter().map(|(_, v)| sum_value(v)).sum(),
    _ => 0,
  }
}

fn sum_avoiding_red(val: &JsonValue) -> i64 {
  if !match val {
    JsonValue::Object(o) => o.iter().any(|(_, v)| match v {
      JsonValue::Short(s) => s == "red",
      JsonValue::String(s) => s == "red",
      _ => false,
    }),
    _ => false,
  } {
    match val {
      JsonValue::Number(n) => n.as_fixed_point_i64(0).unwrap(),
      JsonValue::Array(v) => v.iter().map(sum_avoiding_red).sum(),
      JsonValue::Object(o) => o.iter().map(|(_, v)| sum_avoiding_red(v)).sum(),
      _ => 0,
    }
  } else {
    0
  }
}
