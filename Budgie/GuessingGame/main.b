/* WARNING: This is purely experimental!
There is no Budgie language; this is just me throwing some ideas
around for language design.

*/

from core::io import all;
from core::ord import all;
from core::rand import all;

from GuessingGame::Input import all;

module Main {
    func run_game(answer: nat, total_turns: nat) -> IO () {
        var turns_left = total_turns;

        println("I'm thinking of a number between 1 and 100! Can you guess which one?");
    
        loop {
            if (turns_left == 0) {
                println("You lose!");
                break;
            }

            println("You have {} turn(s) left", turns_left);
            if (turns_left == 1) {
                println("Uh oh! Make it count!!");
            }

            let guess: nat = prompt("Please enter your guess: ",
                (x: nat) -> bool { 1 <= x && x <= 100 }
            );

            println("You guessed: {}.", guess);

            match (guess `compare` answer) {
                Ord::LT => {
                    println("Too low!");
                    dec total_turns;
                }
                Ord::GT => {
                    println("Too high!");
                    dec total_turns;
                }
                Ord::Eq => {
                    println("You win!");
                    break;
                }
            }
        }
    }

    public func main(args: [String]) -> Result<(), std::Error> {
        let die = Uniform::from(1..100); 

        let turn_limit = 10;

        loop {
            run_game(die.sample(), turn_limit);

            match yes_or_no("Do you want to play again?") {
                true => println("Okay, give me a moment to think of a new number!);
                false => {
                    println("Okay, thank you for playing!")
                    break;
                }
            }
        }
    }
}