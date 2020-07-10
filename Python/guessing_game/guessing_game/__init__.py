"""
Runs a number guessing game
in which the player's goal is to guess a number
in the closed interval [1, 100] in 10 turns or less.
"""
__version__ = '0.1.0'
from random import randint

def input_int(prompt, validator=lambda x: True):
    """Prompts the user for an int and checks it against validator."""
    while True:
        try:
            user_input = int(input(prompt))
        except ValueError:
            print("Invalid input. Please try again.")
        else:
            if not validator(user_input):
                print("Invalid input. Please try again.")
            else:
                return user_input


def yes_or_no(prompt):
    """Prompts the user to enter yes or no."""
    while True:
        reply = str(input(prompt + ' (y/n): ')).lower().strip()
        if reply[:1] == 'y':
            return True
        elif reply[:1] == 'n':
            return False
        else:
            print("Invalid input. Please try again.")


def run_game(answer, total_turns):
    """Runs the round of the game."""
    turns_left = total_turns

    print("I'm thinking of a number between 1 and 100! Can you guess which one?")

    while True:
        if turns_left == 0:
            print("You lose!")
            break

        print(f"You have {turns_left} turn(s) left.")
        if turns_left == 1:
            print("Uh oh! Make it count!!")

        guess = input_int("Please enter your guess: ", lambda x: 1 <= x <= 100)

        print(f"You guessed: {guess}.")

        if guess < answer:
            print("Too low!")
            turns_left -= 1
        elif guess > answer:
            print("Too high!")
            turns_left -= 1
        else:
            print("You won!")
            break


def main():
    """Runs the main game loop."""
    turn_limit = 10

    while True:
        run_game(randint(1, 100), turn_limit)

        if yes_or_no("Do you want to play again?"):
            print("Okay, give me a moment to think of a new number!")
        else:
            print("Okay, thank you for playing!")
            break
