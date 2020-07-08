using System;

using static IO.Input;

namespace GuessingGame {

	class Program {
		private static void runGame(int answer, int totalTurns) {
			var turnsLeft = totalTurns;

			Console.WriteLine("I'm thinking of a number between 1 and 100! Can you guess which one?");

			while (true) {
				if (turnsLeft == 0) {
					Console.WriteLine("You lose!");
					break;
				}

				Console.WriteLine($"You have {turnsLeft} turn(s) left.");
				if (turnsLeft == 1) {
					Console.WriteLine("Uh oh! Make it count!!");
				}

				var guess = InputInt("Please enter your guess: ", x => 1 <= x && x <= 100);

				Console.WriteLine($"You guessed: {guess}");

				if (guess < answer) {
					Console.WriteLine("Too low!");
					turnsLeft--;
				} else if (guess > answer) {
					Console.WriteLine("Too high!");
					turnsLeft--;
				} else {
					Console.WriteLine("You win!");
					break;
				}
			}
		}

		static void Main(string[] args) {
			var rnd = new Random();

			const int turnLimit = 10;

			while (true) {
				runGame(rnd.Next(1, 101), turnLimit);

				if (YesOrNo("Do you want to play again? (y/n): ")) {
					Console.WriteLine("Okay, give me a moment to think of a new number!");
				} else {
					Console.WriteLine("Okay, thank you for playing!");
					break;
				}
			}
		}
	}
}
