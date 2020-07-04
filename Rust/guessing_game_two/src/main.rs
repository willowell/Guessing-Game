extern crate rand;
// extern crate typename; <-- for type inspection

use std::io;
use std::cmp::Ordering;

use rand::Rng;
// use typename::TypeName; <-- for type inspection

use exitfailure::ExitFailure;
use failure::ResultExt;

fn main() -> Result<(), ExitFailure> {

    let secret_number = rand::thread_rng().gen_range(1, 101);

    println!("Guess the number!");

    loop {
        println!("Please input your guess.");

        let mut guess = String::new();

        io::stdin().read_line(&mut guess)
            .with_context(|_| format!("Unable to read line! :-("))?;

        // println!("Type of guess before match: {}", guess.type_name_of());
        // output: std::string::String
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Please enter a number! {}", guess);
                continue;
            },
        };

        // println!("Type of guess after match: {}", guess.type_name_of());
        // output: u32
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
