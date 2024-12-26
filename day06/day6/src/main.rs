use std::io;
use indicatif::ProgressBar;

type Coord = (usize, usize);
type Lights = [[(i32, bool); 1000]; 1000];

#[derive(Debug)]
enum Instruction {
    On(Coord, Coord),
    Off(Coord, Coord),
    Toggle(Coord, Coord),
}

fn main() -> Result<(), io::Error> {
    let mut lights = [[(0, false); 1000]; 1000];
    let spinner1: ProgressBar = ProgressBar::new_spinner().with_message("Reading instructions");
    let instrs = read_lines(&spinner1)?;
    let spinner2: ProgressBar = ProgressBar::new_spinner().with_message("Running instructions");
    run_instructions(instrs, &mut lights, &spinner2);
    let spinner3: ProgressBar = ProgressBar::new_spinner().with_message("Counting lights");
    let (part1, part2) = count_lights(&lights, &spinner3);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
    Ok(())
}

fn count_lights(lights: &Lights, spinner: &ProgressBar) -> (i32, i32) {
    let mut total1 = 0;
    let mut total2 = 0;
    for col in lights {
        for light in col {
            if light.1 { total1 += 1 }
            total2 += light.0;
            spinner.inc(1);
        }
    }
    (total1, total2)
}

#[allow(unused)]
fn display_lights(lights: &Lights) {
    let mut rows = vec![String::new(); 1000];
    for col in lights {
        for (idx, light) in col.iter().enumerate() {
            rows[idx].push(if light.1 { '#' } else { '.' });
        }
    }
    for row in rows {
        println!("{}", row);
    }
}

fn run_instructions(instrs: Vec<Instruction>, lights: &mut Lights, spinner: &ProgressBar) {
    let mut counter = 0;
    for instr in instrs {
        match instr {
            Instruction::On(coord1, coord2) => {
                turn_on_lights(coord1, coord2, lights);
                counter += 1;
                spinner.inc(1);
                spinner.set_message(format!("Running instructions - On {:?} - {:?} ; {}", coord1, coord2, counter));
            },
            Instruction::Off(coord1, coord2) => {
                turn_off_lights(coord1, coord2, lights);
                counter += 1;
                spinner.inc(1);
                spinner.set_message(format!("Running instructions - Off {:?} - {:?} ; {}", coord1, coord2, counter));
            },
            Instruction::Toggle(coord1, coord2) => {
                toggle_lights(coord1, coord2, lights);
                counter += 1;
                spinner.inc(1);
                spinner.set_message(format!("Running instructions - Toggle {:?} - {:?} ; {}", coord1, coord2, counter));
            },
        }
    }
}

fn turn_on_lights(coord1: Coord, coord2: Coord, lights: &mut Lights) {
    for x in coord1.0..(coord2.0 + 1) {
        for y in coord1.1..(coord2.1 + 1) {
            lights[x][y].0 += 1;
            lights[x][y].1 = true;
        }
    }
}

fn turn_off_lights(coord1: Coord, coord2: Coord, lights: &mut Lights) {
    for x in coord1.0..(coord2.0 + 1) {
        for y in coord1.1..(coord2.1 + 1) {
            lights[x][y].0 -= 1;
            if lights[x][y].0 < 0 { lights[x][y].0 = 0 }
            lights[x][y].1 = false;
        }
    }
}

fn toggle_lights(coord1: Coord, coord2: Coord, lights: &mut Lights) {
    for x in coord1.0..(coord2.0 + 1) {
        for y in coord1.1..(coord2.1 + 1) {
            lights[x][y].0 += 2;
            lights[x][y].1 = !lights[x][y].1;
        }
    }
}

fn read_lines(spinner: &ProgressBar) -> Result<Vec<Instruction>, io::Error> {
    let mut instrs = Vec::new();
    for line in io::stdin().lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(' ').collect();
        instrs.push(match parts[0] {
            "turn" => {
                let parts1: Vec<usize> = parts[2].split(',').map(|i| i.parse::<usize>().unwrap()).collect();
                let coords1 = (parts1[0], parts1[1]);
                let parts2: Vec<usize> = parts[4].split(',').map(|i| i.parse::<usize>().unwrap()).collect();
                let coords2 = (parts2[0], parts2[1]);
                match parts[1] {
                    "on" => Instruction::On(coords1, coords2),
                    "off" => Instruction::Off(coords1, coords2),
                    _ => panic!("Invalid input!"),
                }
            },
            "toggle" => {
                let parts1: Vec<usize> = parts[1].split(',').map(|i| i.parse::<usize>().unwrap()).collect();
                let coords1 = (parts1[0], parts1[1]);
                let parts2: Vec<usize> = parts[3].split(',').map(|i| i.parse::<usize>().unwrap()).collect();
                let coords2 = (parts2[0], parts2[1]);
                Instruction::Toggle(coords1, coords2)
            },
            _ => panic!("Invalid input!"),
        });
        spinner.inc(1);
    }
    Ok(instrs)
}
