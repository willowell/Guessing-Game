//
// Created by William Howell on 2020-05-16.
//

#ifndef GUESSINGGAME_CCTYPE_WRAPPERS_HPP
#define GUESSINGGAME_CCTYPE_WRAPPERS_HPP

#include <cctype>

namespace Input::CCType {
	bool IsAlNum(char ch) {
		return std::isalnum(static_cast<unsigned char>(ch));
	}
	
	bool IsAlpha(char ch) {
		return std::isalpha(static_cast<unsigned char>(ch));
	}
	
	bool IsLower(char ch) {
		return std::islower(static_cast<unsigned char>(ch));
	}
	
	bool IsUpper(char ch) {
		return std::isupper(static_cast<unsigned char>(ch));
	}
	
	bool IsDigit(char ch) {
		return std::isdigit(static_cast<unsigned char>(ch));
	}
	
	bool IsXDigit(char ch) {
		return std::isxdigit(static_cast<unsigned char>(ch));
	}
	
	bool IsCntrl(char ch) {
		return std::iscntrl(static_cast<unsigned char>(ch));
	}
	
	bool IsGraph(char ch) {
		return std::isgraph(static_cast<unsigned char>(ch));
	}
	
	bool IsSpace(char ch) {
		return std::isspace(static_cast<unsigned char>(ch));
	}
	
	bool IsBlank(char ch) {
		return std::isblank(static_cast<unsigned char>(ch));
	}
	
	bool IsPrint(char ch) {
		return std::isprint(static_cast<unsigned char>(ch));
	}
	
	bool IsPunct(char ch) {
		return std::ispunct(static_cast<unsigned char>(ch));
	}
	
	char ToLower(char ch) {
		return static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
	}
	
	char ToUpper(char ch) {
		return static_cast<char>(std::toupper(static_cast<unsigned char>(ch)));
	}
}

#endif //GUESSINGGAME_CCTYPE_WRAPPERS_HPP
