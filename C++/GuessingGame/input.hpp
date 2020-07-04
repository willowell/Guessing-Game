//
// Created by William Howell on 2020-07-03.
//

#ifndef GUESSINGGAME_INPUT_HPP
#define GUESSINGGAME_INPUT_HPP

#include <functional>
#include <iostream>
#include <string>
#include <utility>

#include "cctype_wrappers.hpp"

namespace Input {
	/**
	 * Prints a prompt to STDOUT, reads in a T value from STDIN, and validates the input using `validator`
	 * 	as a predicate.
	 * @tparam T The type of the user's input.
	 * @param prompt Message to display to user.
	 * @param validator return-type: bool argument-type: T A lambda that determines the predicate on the input.
	 * @return A T value, valid for the given predicate.
	 *
	 *
	 */
	template<typename T>
	T Input(const std::string & prompt, std::function<bool(T)> validator) {
		T user_input{};
		
		// Shorthand to clean up the for-loop.
		auto prompt_for_input = [&] () {
			std::cout << prompt;
			std::cin >> user_input;
		};
		
		for (prompt_for_input(); !validator(user_input); prompt_for_input()) {
			
			std::cout << "Invalid input. Please try again." << std::endl;
			std::cin.clear();
			std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		}
		std::cin.clear();
		std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		
		return user_input;
	}
	
	int InputInt(const std::string & prompt, std::function<bool(int)> validator) {
		return Input<int>(prompt, std::move(validator));
	}
	
	long InputLong(const std::string & prompt, std::function<bool(long)> validator) {
		return Input<long>(prompt, std::move(validator));
	}
	
	double InputDouble(const std::string & prompt, std::function<bool(double)> validator) {
		return Input<double>(prompt, std::move(validator));
	}
	
	char InputChar(const std::string & prompt, std::function<bool(char)> validator =
	[] (char arg) -> bool { return CCType::IsAlpha(arg); }) {
		return Input<char>(prompt, std::move(validator));
	}
	
	std::string InputString(const std::string & prompt) {
		return Input<std::string>(prompt, [] (const std::string& arg) -> bool { return true; });
	}
	
	std::string InputString(const std::string & prompt, std::function<bool(std::string)> validator) {
		return Input<std::string>(prompt, std::move(validator));
	}
	
	
	bool YesOrNoPrompt(const std::string& prompt) {
		std::string buffer;
		char answer = 'a';
		
		answer = InputChar(prompt, [] (char arg) -> bool {
			arg = CCType::ToUpper(arg);
			return (arg == 'Y' || arg == 'N');
		});
		answer = CCType::ToUpper(answer);
		return (answer == 'Y');
	}
}

#endif //GUESSINGGAME_INPUT_HPP
