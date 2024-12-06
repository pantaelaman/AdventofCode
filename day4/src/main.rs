fn main() {
  let input = std::env::args().nth(1).unwrap();

  let mut p1_found = false;
  for i in 1.. {
    let hash = md5::compute(format!("{}{}", input, i));
    if hash[0] == 0 && hash[1] == 0 {
      if !p1_found && hash[2] <= 0x0f {
        p1_found = true;
        println!("Part 1: {}", i);
      } else if hash[2] == 0 {
        println!("Part 2: {}", i);
        break;
      }
    }
  }
}
