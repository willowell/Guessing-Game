import Foundation
import Runes

func runGame(answer: UInt32, totalTurns: UInt32) {
    var turnsLeft = totalTurns

    print("I'm thinking of a number between 1 and 100! Can you guess which one?")

    while true {
        if turnsLeft == 0 {
            print("You lose!")
            break
        }

        print("You have \(turnsLeft) turn(s) left.")
        if turnsLeft == 1 {
            print("Uh oh! Make it count!!")
        }

        let guess: UInt32 = prompt(
            msg: "Please enter your guess: ",
            validator: { (arg: UInt32) -> Bool in 1 <= arg && arg <= 100 }
        )

        print("You guessed \(guess).")

        if guess < answer {
            print("Too low!")
            turnsLeft -= 1
        } else if guess > answer {
            print("Too high!")
            turnsLeft -= 1
        } else {
            print("You win!")
            break
        }

    }
}

public func main() {
    let turnLimit: UInt32 = 10

    while true {
        runGame(answer: UInt32.random(in: 1...100), totalTurns: turnLimit)

        switch yesOrNo(msg: "Do you want to play again?") {
            case true:
                print("Okay, give me a moment to think of a new number!")
                break
            case false:
                print("Okay, thank you for playing!")
                return
        }
    }
}

main()