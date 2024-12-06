fn main() {
  let input = std::env::args().nth(1).unwrap();

  let mut p1 = 0;
  let mut i = 1;
  loop {
    let hash = md5::compute(format!("{}{}", input, i));
    if hash[0] == 0 && hash[1] == 0 {
      if p1 == 0 && hash[2] <= 0x0f {
        p1 = i;
        println!("Part 1: {}", p1);
      } else if hash[2] == 0 {
        break;
      }
    }
    i += 1;
  }

  println!("Part 2: {}", i);
}
