use std::collections::HashMap;
use indicatif::ProgressBar;

type Wires = HashMap<String, Wire>;

enum Wire {
    Valued(u16),
    Unvalued(Instruction),
}

impl Wire {
    pub fn value(self, wires: &mut Wires) -> (u16, Wire) {
        match self {
            Self::Valued(v) => (v, self),
            Self::Unvalued(instr) => {
                let value = instr.resolve(wires);
                (value, Self::Valued(value))
            },
        }
    }
}

#[derive(Debug, Clone)]
enum Instruction {
    CPY{
        src: Valued,
    },
    NOT{
        src: Valued,
    },
    AND{
        src1: Valued,
        src2: Valued,
    },
    OR{
        src1: Valued,
        src2: Valued,
    },
    LSF{
        src1: Valued,
        src2: Valued,
    },
    RSF{
        src1: Valued,
        src2: Valued,
    },
}

impl Instruction {
    fn resolve(self, wires: &mut Wires) -> u16 {
        use Instruction::*;
        match self {
            CPY{src} => src.get_value(wires),
            NOT{src} => !src.get_value(wires),
            AND{src1, src2} => src1.get_value(wires) & src2.get_value(wires),
            OR{src1, src2} => src1.get_value(wires) | src2.get_value(wires),
            LSF{src1, src2} => src1.get_value(wires) << src2.get_value(wires),
            RSF{src1, src2} => src1.get_value(wires) >> src2.get_value(wires),
        }
    }
}

#[derive(Debug, Clone)]
enum Valued {
    Wire{
        name: String,
    },
    Literal{
        value: u16,
    },
}

impl Valued {
    pub fn get_value(self, wires: &mut Wires) -> u16 {
        match self {
            Valued::Wire{ name } => {
                let wire = wires.remove(&name).unwrap();
                let (value, product) = wire.value(wires);
                wires.insert(name, product);
                value
            },
            Valued::Literal { value } => value,
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    let stdin = std::io::stdin().lines().map(|i| i.unwrap()).collect::<Vec<String>>();
    let spinner1 = ProgressBar::new_spinner().with_message("Reading instructions");
    let mut wires = read_lines(&stdin,&spinner1)?;
    let initial_wire = wires.remove("a").unwrap();
    let (part1, dead_wire) = initial_wire.value(&mut wires);
    println!("Part 1: {}", part1);
    std::mem::drop(wires);
    let spinner2 = ProgressBar::new_spinner().with_message("Reading instructions");
    let mut wires = read_lines(&stdin, &spinner2)?;
    wires.insert("b".to_string(), dead_wire);
    let initial_wire2 = wires.remove("a").unwrap();
    println!("Part 2: {}", initial_wire2.value(&mut wires).0);
    Ok(())
}

fn read_lines(input: &Vec<String>, spinner: &ProgressBar) -> Result<Wires, std::io::Error> {
    let mut wires = Wires::new();
    let mut counter = 0;
    for line in input {
        let parts = line.split("->").collect::<Vec<&str>>();
        let dest = parts[1][1..].to_string();
        let src_parts = parts[0].split(' ').map(|p| p.to_string()).filter(|e| e != &"").collect::<Vec<String>>();
        wires.insert(dest, Wire::Unvalued(read_instr(src_parts)));
        counter += 1;
        spinner.inc(1);
        spinner.set_message(format!("Reading instructions - {}", counter + 1));
    }
    Ok(wires)
}

fn read_instr(src_parts: Vec<String>) -> Instruction {
    if src_parts[0] == "NOT" {
        Instruction::NOT {src: read_valued(&src_parts[1])}
    } else if src_parts.len() > 1 {
        let src1 = read_valued(&src_parts[0]);
        let src2 = read_valued(&src_parts[2]);
        match src_parts[1].as_str() {
            "AND" => Instruction::AND {src1, src2},
            "OR" => Instruction::OR {src1, src2},
            "LSHIFT" => Instruction::LSF {src1, src2},
            "RSHIFT" => Instruction::RSF {src1, src2},
            _ => panic!("Invalid from parts: {:?}", src_parts),
        }
    } else {
        Instruction::CPY {src: read_valued(&src_parts[0])}
    }
}

fn read_valued(string: &str) -> Valued {
    match string.parse::<u16>() {
        Ok(v) => return Valued::Literal { value: v },
        Err(_) => return Valued::Wire { name: string.to_string() },
    }
}
