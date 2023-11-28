package lib

import lib.input.*
import lib.read.given
import lib.math.ordering.{given, *}

def askForNumber(answer: Int, totalTurns: Int): Unit =
  def go(turnsLeft: Int): Unit =
    if turnsLeft == 0 then
      println("You lose!")
    else
      println(s"You have $turnsLeft turn(s) left.")
      if turnsLeft == 1 then println("Uh oh! Make it count!!")

      val number = prompt[Int]("Please enter your guess: ", (x) => 1 to 100 contains x)

      println(s"You guessed $number")

      number <=> answer match
        case Ordering.LT =>
          println("Too low!")
          go(turnsLeft - 1)
        case Ordering.GT =>
          println("Too high!")
          go(turnsLeft - 1)
        case Ordering.EQ =>
          println("You win!")
  
  go(totalTurns)

def runGame(totalTurns: Int): Unit =
  println("I'm thinking of a number between 1 and 100! Can you guess which one?")

  val randomNumber: Int = scala.util.Random.between(1, 101)

  askForNumber(randomNumber, totalTurns)

  val continue = yesOrNo("Do you want to play again?")

  if continue then
    println("Okay, give me a moment to think of a new number!")
    runGame(totalTurns)
  else
    println("Okay, thank you for playing!")
