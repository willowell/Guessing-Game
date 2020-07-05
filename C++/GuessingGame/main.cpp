#include <iostream>
#include <random>

#include "input.hpp"

void runGame(int answer, int total_turns) {
	int turns_left = total_turns;

	std::cout << "I'm thinking of a number between 1 and 100! Can you guess which one?\n";

	while (true) {
		if (turns_left == 0) {
			std::cout << "You lose!\n";
			break;
		}
		std::cout << "You have " << turns_left << " turn(s) left.\n";
		if (turns_left == 1) {
			std::cout << "Uh oh! Make it count!!\n";
		}

		int guess = Input::InputInt("Please enter your guess: ",
			[] (int arg) -> bool { return (1 <= arg && arg <= 100); }
		);
		
		std::cout << "You guessed: " << guess << '\n';
		
		if (guess < answer) {
			std::cout << "Too low!\n";
			turns_left--;
		} else if (guess > answer) {
			std::cout << "Too high!\n";
			turns_left--;
		} else {
			std::cout << "You won!\n";
			break;
		}
	}
}

int main() {
	std::random_device rand_dev;
	std::mt19937 gen(rand_dev());
	std::uniform_int_distribution<> dist(1, 100);

	int turn_limit = 10;
	
	while (true) {
		runGame(dist(gen), turn_limit);
		
		if (Input::YesOrNoPrompt("Do you want to play again? (y/n): ")) {
			std::cout << "Okay, give me a moment to think of a new number!\n";
		} else {
			std::cout << "Okay, thank you for playing!\n";
			break;
		}
	}
	
	return 0;
}
