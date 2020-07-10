extern crate rand;

use std::io;
use std::cmp::Ordering;

use exitfailure::ExitFailure;
use failure::ResultExt;
use rand::Rng;

fn main() -> Result<(), ExitFailure> {

    let secret_number = rand::thread_rng().gen_range(1, 101);

    println!("Guess the number!");

    loop {
        println!("Please input your guess.");

        let mut guess = String::new();

        io::stdin().read_line(&mut guess)
            .with_context(|_| format!("Unable to read line! :-("))?;

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Please enter a number! {}", guess);
                continue;
            },
        };

        println!("You guessed: {}", guess);

        match guess.cmp(&secret_number) {
            Ordering::Less =>    println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal =>   {
                println!("You win!");
                break;
            }
        }
    }
    Ok(())
}
