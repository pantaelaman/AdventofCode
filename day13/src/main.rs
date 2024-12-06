use std::{
  collections::HashMap,
  fs::File,
  io::{stdout, Write},
};

use crossterm::{
  cursor,
  event::{self, KeyEventKind, KeyModifiers},
  execute, queue, terminal, ExecutableCommand,
};
use intcode::{
  file_to_values, OwnedContext, Program, ProgrammedInput, SingletonValue, Value,
};

fn main() {
  let file = File::open(std::env::args().nth(1).unwrap()).unwrap();
  let values = file_to_values(file);

  let mut stdout = stdout();
  terminal::enable_raw_mode().unwrap();
  execute!(
    stdout,
    terminal::EnterAlternateScreen,
    terminal::Clear(terminal::ClearType::All)
  )
  .unwrap();

  let context =
    OwnedContext::new(ProgrammedInput::new([]), SingletonValue::from(0));

  let mut program = Program::new(values.clone(), context);
  let mut grid: HashMap<(Value, Value), Value> = HashMap::new();
  while !program.is_complete() {
    program.run_until_interrupt();
    let x = *program.context.output;
    program.run_until_interrupt();
    let y = *program.context.output;
    program.run_until_interrupt();
    let tile = *program.context.output;

    grid.insert((x, y), tile);
  }

  let num_blocks = grid.values().filter(|tile| **tile == 2).count();
  println!("Part 1: {}", num_blocks);

  let (maxx, maxy): (Value, Value) = grid
    .keys()
    .fold((-1, -1), |(ax, ay), (x, y)| (ax.max(*x), ay.max(*y)));
  let maxx = maxx as u16;
  let maxy = maxy as u16;

  let mut pipe = || -> Value {
    let mut stdout = std::io::stdout();
    execute!(stdout, cursor::MoveTo(0, maxy + 1)).unwrap();
    print!("Requesting input");
    stdout.flush().unwrap();
    let val = loop {
      match event::read().unwrap() {
        event::Event::Key(event::KeyEvent { code, .. }) => {
          break match code {
            event::KeyCode::Char('a') => -1,
            event::KeyCode::Char('s') => 1,
            _ => 0,
          }
        }
        _ => continue,
      }
    };
    execute!(stdout, cursor::MoveTo(0, maxy + 1)).unwrap();
    print!("                ");
    execute!(stdout, cursor::MoveTo(0, maxy + 1)).unwrap();
    print!("Input {}", val);
    stdout.flush().unwrap();
    val
  };
  let context = OwnedContext::new(
    &mut pipe as &mut dyn FnMut() -> Value,
    SingletonValue::from(0),
  );

  let mut game_values = values.clone();
  game_values[0] = 2;
  let mut program = Program::new(game_values, context);

  while !program.is_complete() {
    program.run_until_interrupt();
    let x = *program.context.output;
    program.run_until_interrupt();
    let y = *program.context.output;
    program.run_until_interrupt();
    let tile = *program.context.output;

    if (x, y) == (-1, 0) {
      execute!(stdout, cursor::MoveTo(maxx + 1, 0)).unwrap();
      print!("{}", tile);
      continue;
    }

    execute!(stdout, cursor::MoveTo(x as u16, y as u16)).unwrap();
    print!(
      "{}",
      match tile {
        0 => '.',
        1 => '\u{2588}',
        2 => '#',
        3 => '—',
        4 => 'O',
        _ => unimplemented!(),
      }
    );
  }
}
