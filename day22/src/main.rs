use std::{
  collections::{HashMap, HashSet, VecDeque},
  io::stdin,
};

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Player {
  deck: VecDeque<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PlayerID {
  P1,
  P2,
}

fn main() {
  let lines = stdin().lines().map(|line| line.unwrap()).collect_vec();
  // let contents = lines.join("\n");

  let (p1, p2) =
    lines.split_at(lines.iter().position(|line| line.is_empty()).unwrap());

  let p1_key = Player {
    deck: p1
      .iter()
      .skip(1)
      .map(|line| line.parse().unwrap())
      .rev()
      .collect(),
  };
  let p2_key = Player {
    deck: p2
      .iter()
      .skip(2)
      .map(|line| line.parse().unwrap())
      .rev()
      .collect(),
  };

  // TOP is BACK, BOTTOM is FRONT

  let mut p1 = p1_key.clone();
  let mut p2 = p2_key.clone();

  let mut round = 1;
  let winner = loop {
    let p1_card = p1.deck.pop_back().unwrap();
    let p2_card = p2.deck.pop_back().unwrap();

    if p1_card > p2_card {
      p1.deck.push_front(p1_card);
      p1.deck.push_front(p2_card);

      if p2.deck.is_empty() {
        break &p1;
      }
    } else {
      p2.deck.push_front(p2_card);
      p2.deck.push_front(p1_card);

      if p1.deck.is_empty() {
        break &p2;
      }
    }

    round += 1;
  };

  let score: usize = winner
    .deck
    .iter()
    .enumerate()
    .map(|(i, card)| (i + 1) * card)
    .sum();

  println!("Part 1: {}", score);

  let mut p1 = p1_key.clone();
  let mut p2 = p2_key.clone();
  let mut cache = HashMap::new();
  let winner = recursive_combat(&mut p1, &mut p2, &mut cache, &mut 1);

  //println!("\n== Post-game results ==");
  //println!("Player 1's deck: {:?}", p1.deck);
  //println!("Player 2's deck: {:?}", p2.deck);

  let score: usize = match winner {
    PlayerID::P1 => p1.deck,
    PlayerID::P2 => p2.deck,
  }
  .iter()
  .enumerate()
  .map(|(i, card)| (i + 1) * card)
  .sum();

  println!("Part 2: {}", score);
}

// depth for debugging only
fn recursive_combat(
  p1_deck: &mut Player,
  p2_deck: &mut Player,
  cache: &mut HashMap<(Player, Player), PlayerID>,
  game: &mut usize,
) -> PlayerID {
  let mut seen = HashSet::new();

  let curgame = *game;
  //println!("=== Game {curgame} ===\n");

  let mut round = 1;
  let winner = loop {
    //println!("-- Round {round} Game {curgame} --");
    //println!("Player 1's deck: {:?}", p1_deck.deck);
    //println!("Player 2's deck: {:?}", p2_deck.deck);
    let st = (p1_deck.clone(), p2_deck.clone());

    if *game != 1 {
      if let Some(winner) = cache.get(&st) {
        break *winner;
      }
    }

    if !seen.insert(st) {
      //println!("Already seen this, player 1 wins!\n");
      break PlayerID::P1;
    }

    let p1_card = p1_deck.deck.pop_back().unwrap();
    let p2_card = p2_deck.deck.pop_back().unwrap();

    //println!("Player 1 plays: {}", p1_card);
    //println!("Player 2 plays: {}", p2_card);

    let p1_len = p1_deck.deck.len();
    let p2_len = p2_deck.deck.len();
    let p1_sufficient = p1_len >= p1_card;
    let p2_sufficient = p2_len >= p2_card;

    let winner = if p1_sufficient && p2_sufficient {
      //println!("Playing a sub-game to determine the winner...\n");
      *game += 1;
      let winner = recursive_combat(
        &mut Player {
          deck: p1_deck.deck.make_contiguous()[p1_len - p1_card..]
            .iter()
            .copied()
            .collect(),
        },
        &mut Player {
          deck: p2_deck.deck.make_contiguous()[p2_len - p2_card..]
            .iter()
            .copied()
            .collect(),
        },
        cache,
        game,
      );

      //println!("...anyway, back to game {curgame}");
      winner
    } else {
      if p1_card > p2_card {
        PlayerID::P1
      } else {
        PlayerID::P2
      }
    };

    match winner {
      PlayerID::P1 => {
        //println!("Player 1 wins round {round} of game {curgame}!\n");
        p1_deck.deck.push_front(p1_card);
        p1_deck.deck.push_front(p2_card);

        if p2_deck.deck.is_empty() {
          break PlayerID::P1;
        }
      }
      PlayerID::P2 => {
        //println!("Player 2 wins round {round} of game {curgame}!\n");
        p2_deck.deck.push_front(p2_card);
        p2_deck.deck.push_front(p1_card);

        if p1_deck.deck.is_empty() {
          break PlayerID::P2;
        }
      }
    }

    round += 1;
  };

  //println!(
  //  "The winner of game {curgame} is {}!\n",
  //  match winner {
  //    PlayerID::P1 => "player 1",
  //    PlayerID::P2 => "player 2",
  //  }
  //);

  cache.extend(seen.into_iter().map(|st| (st, winner)));

  winner
}
