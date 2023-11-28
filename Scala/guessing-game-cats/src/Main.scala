package guessinggame

import guessinggame.input.*
import guessinggame.read.{given}

import cats.*
import cats.effect.*
import cats.kernel.Comparison
import cats.syntax.all.*
import cats.effect.std.Random

extension[A: Order](lhs: A) {
  def <=>(rhs: A): Comparison = Order[A].comparison(lhs, rhs)
}

def askForNumber(answer: Int, totalTurns: Int): IO[Unit] = {
  def go(turnsLeft: Int): IO[Unit] = {
    if turnsLeft == 0 then
      IO.println("You lose!")
    else for {
      _ <- IO.println(
        s"You have $turnsLeft turn(s) left" |+| (if turnsLeft == 1 then "Uh oh! Make it count!!" else "")
      )

      number <- prompt[Int]("Please enter your guess: ") { x => 1 <= x && x <= 100 }

      _ <- IO.println(s"You guessed $number")

      _ <- (number <=> answer) match {
        case Comparison.LessThan => {
          IO.println("Too low!") >> go(turnsLeft - 1)
        }
        case Comparison.GreaterThan => {
          IO.println("Too high!") >> go(turnsLeft - 1)
        }
        case Comparison.EqualTo => {
          IO.println("You win!")
        }
      }
    } yield ()
  }

  go(totalTurns)
}

def runGame(gen: Random[IO], totalTurns: Int): IO[Unit] = for {
  _ <- IO.println("I'm thinking of a number between 1 and 100! Can you guess which one?")

  randomNumber <- gen.betweenInt(1, 101)

  _ <- askForNumber(randomNumber, totalTurns)

  continue <- yesOrNo("Do you want to play again?")

  _ <- if continue then
    IO.println("Okay, give me a moment to think of a new number!")
    >> runGame(gen, totalTurns)
  else
    IO.println("Okay, thank you for playing!")
} yield ()

object Main extends IOApp.Simple {
  val turnLimit = 10

  override final val run: IO[Unit] = for {
    gen <- Random.scalaUtilRandom[IO]

    _ <- runGame(gen, turnLimit)
  } yield ()
}
