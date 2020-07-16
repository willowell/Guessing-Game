use std::cmp::Ordering;

use promptor::Promptor;

use rand::distributions::{Distribution, Uniform};

fn run_game(answer: u32, total_turns: u32, promptor: &mut Promptor<std::io::StdinLock, std::io::Stdout>) {
    let mut turns_left = total_turns;

    println!("I'm thinking of a number between 1 and 100! Can you guess which one?");

    loop {
        if turns_left == 0 {
            println!("You lose!");
            break;
        }

        println!("You have {} turn(s) left.", turns_left);
        if turns_left == 1 {
            println!("Uh oh! Make it count!!");
        }

        let guess: u32 = promptor.prompt("Please enter your guess: ", |x| 1 <= x && x <= 100);

        println!("You guessed: {}.", guess);

        match guess.cmp(&answer) {
            Ordering::Less => {
                println!("Too low!");
                turns_left -= 1;
            }
            Ordering::Greater => {
                println!("Too high!");
                turns_left -= 1;
            }
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}

fn main() {
    let stdio = std::io::stdin();
    let input = stdio.lock();
    let output = std::io::stdout();

    let mut promptor = Promptor {
        reader: input,
        writer: output
    };

    let mut rng = rand::thread_rng();
    let die = Uniform::from(1..=100);

    let turn_limit = 10;

    loop {
        run_game(die.sample(&mut rng), turn_limit, &mut promptor);

        let again: char = promptor.prompt("Do you want to play again? (y/n): ", |x| x == 'y' || x == 'n');

        let again = if again == 'y' { true } else { false };

        match again {
            true => println!("Okay, give me a moment to think of a new number!"),
            false => {
                println!("Okay, thank you for playing!");
                break;
            }
        }
    }
}
