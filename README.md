# The Number Guessing Game
### in Several Programming Languages
---
Hello there! This is a collection of implementations of the Guessing Game example program from the Rust Book, with a few modifications. The programming languages I have chosen are my favourite languages.
This is mostly for fun, but hopefully this will prove interesting to people who are curious about the differences in coding in different languages.

---
#### Game Structure
Objective: the computer randomly selects a number in the closed interval [1, 100]. The player must correctly guess this number in 10 turns or less to win. The player may play as many rounds as they like.

Each time the player guesses a number, the program tells the player if the number is too low or too high and informs the player of how many turns they have left.

---
#### Common Features
* Emphasis on a functional programming style.
* A polymorphic `input` function handles user input and has the general form: `input(prompt, validator)`.
* A monomorphized `inputInt` function based on the polymorphic `input` function.
* A `yesOrNo` function that handles asking the player if they want to play again.
* Error-handling is done through `Result`, `Maybe`, `Option(al)`, etc. *only where possible*.

---
#### Notes on the Separate Implementations
##### C++
You will need a C++20-enabled compiler and CMake. I made the C++ version using CLion, but since there are only four files and no external dependencies, you should have no trouble compiling it on just the command line.

##### D
Note that this implementation was made with `DMD64 D Compiler v2.092.1`. You will also need to add `optional` via dub.

##### Haskell (Reference Implementation)
This is the reference implementation. The behaviour of this program acts as the specification for the behaviour of the other implementations. 
I chose the Haskell version as the reference implementation because I am learning Haskell and because the Haskell implementation is by far the *shortest*; it is very easy to refer back to it to see how a particular function should behave in the other languages. The type signatures on the Haskell functions also act as a nice sanity check. For instance, I used the `promptLine` function in the Haskell implementation to check the behaviour of the `promptLine` function in the D implementation.
I made this implementation with Stack. Simply `cd` into the directory, run `stack build`, and then run `stack run` or load it via `stack ghci` :-)

##### TypeScript
To run this version, `cd` into the directory, run `yarn`, and then run `yarn start`.
This implementation depends on `lodash` and `purify-ts`.