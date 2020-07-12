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
* No dependencies *or* if there are dependencies, they are handled on the project level.

---
#### Notes on the Separate Implementations
##### C#
Note that I made this implementation with Visual Studio for Mac using the dotnet CLI tool. The .NET Core version is 3.1.301. I did not use any NuGet packages.

##### C++20
You will need a C++20-enabled compiler and CMake. I made the C++ version using CLion, but since there are only four files and no external dependencies, you should have no trouble compiling it on just the command line.

##### D
Note that I made this implementation with `DMD64 D Compiler v2.092.1`. You will also need to add `optional` via dub.

##### Haskell (Reference Implementation)
This is the reference implementation. The behaviour of this program acts as the specification for the behaviour of the other implementations. 
I chose the Haskell version as the reference implementation because I am learning Haskell and because the Haskell implementation is by far the *shortest*; it is very easy to refer back to it to see how a particular function should behave in the other languages. The type signatures on the Haskell functions also act as a nice sanity check. For instance, I used the `promptLine` function in the Haskell implementation to check the behaviour of the `promptLine` function in the D implementation.
I made this implementation with Stack. Simply `cd` into the directory, run `stack build`, and then run `stack run` or load it via `stack ghci` :-)

##### Python 3
To run this implementation, you will need to install [poetry](https://python-poetry.org/), which was the closest package manager I could find to `npm` or `cargo` for Python. I really enjoyed the UX of `poetry` and how easy it was to use, so I certainly hope you give it a try!

Anyway, just `cd` into the directory, and `poetry install && poetry build`, and then run `poetry run start` when you are ready :-)

If you do not have `poetry`, that's okay - you can certainly copy `main.py` and the `guessing_game` module and run them with your preferred method. This implementation has only `pylint` as an external dependency.

##### Rust
To run this implementation, simply [install Rust](https://www.rust-lang.org/), and then `cd` into the directory and run `cargo build`. Cargo will take care of the dependencies for you. Once `cargo build` is done, just run `cargo run` when you are ready. :-) 

##### TypeScript
To run this implementation, you will need Node 14.4.0 or above and yarn 1.22.4 or above.
Then `cd` into the directory, run `yarn`, and then run `yarn start`.
This implementation depends on `prompts` and `random`, in addition to several dev dependencies, including TypeScript. This implementation adheres to Standard JS style.

---
#### Observations
In the process of writing these implementations, I found:
* With `optional`, D lands halfway between the C++ and Haskell implementations; the code in the D version looks (unsurprisingly) like the C++ code, but in other places, like `promptLine`, it looks almost like the Haskell code.
* I found the C# implementation to be quite gnarly to make! I had trouble figuring out how to make a polymorphic `read`/`input` function because I didn't know enough about how C# handles generic programming and reflection. I figure that a better answer will involve reflection and/or constraining the type on `IConvertible`, but I need to research this.
* I finally discovered the nightmare that is dealing with project-level dependencies in Python! I also had to do some plumbing on my Mac - it turns out I had several versions of Python in different places from installing stuff with Homebrew, Anaconda, and installing Python itself through the website! The Python implementation is pretty straightforward since Python already has an `input` function. I didn't feel like introducing a `Maybe` monad to the Python version.
* 

---
#### Why Not (C, Java, Kotlin, Swift, etc.)?
Mostly because I just didn't feel like it.
