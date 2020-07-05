import std.datetime;
import std.random;
import std.stdio;

import optional;

import input : input, inputInt, read, promptLine, yesOrNo;

void runGame(int answer, int totalTurns) {
	int turnsLeft = totalTurns;

	writeln("I'm thinking of a number between 1 and 100! Can you guess which one?");

	while (true) {
		if (turnsLeft == 0) {
			writeln("You lose!");
			break;
		}

		writeln("You have ", turnsLeft, " turn(s) left.");
		if (turnsLeft == 1) {
			writeln("Uh oh! Make it count!!");
		}

		immutable int guess = inputInt("Please enter your guess: ", (int x) => 1 <= x && x <= 100);

		writeln("You guessed: ", guess);

		if (guess < answer) {
			writeln("Too low!");
			turnsLeft--;
		} else if (guess > answer) {
			writeln("Too high!");
			turnsLeft--;
		} else {
			writeln("You win!");
			break;
		}
	}
}

void main() {
	auto now = Clock.currTime(UTC());
	Random rand = Random(now.second);

	int turnLimit = 10;

	while (true) {
		runGame(uniform(1, 101, rand), turnLimit);

		if (yesOrNo("Do you want to play again? (y/n): ")) {
			writeln("Okay, give me a moment to think of a new number!");
		} else {
			writeln("Okay, thank you for playing!");
			break;
		}
	}
}
