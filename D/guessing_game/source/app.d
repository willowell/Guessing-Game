import std.datetime;
import std.random;
import std.stdio;
import std.string;     // -> strip
import std.conv;       //-> to!()
import std.exception;  //-> The exceptions

void main() {

	auto now = Clock.currTime(UTC());
    auto seed = now.second;
    auto rand = Random(seed);

	int secret_number = uniform(1, 101, rand);

	int turn_limit = 10;

	int user_turns = 0;

	writeln("Guess the number in ten turns or less!");

	while (true) {
		if (user_turns == turn_limit) {
			writeln("You lose! :-( Please play again!");
			break;
		}

		writeln("Please input your guess.");
		writeln("You have ", (turn_limit - user_turns), " turn(s) left.");
		if (user_turns == (turn_limit - 1)) {
			writeln("Uh oh! Make it count!!");
		}
		stdout.flush();

		string guess;

		try {
			guess = stdin.readln();
		} catch (StdioException e) {
			// An I/O error has occurred!
			writeln(e.msg);
		} catch (Exception e) {
			// Some other error has occurred!
			writeln(e.msg);
		}

		guess = strip(guess);
		
		int guessed_number = 0;

		// Analyze the contents of the string.
		// If it is numeric,
		// then try converting it to an int.
		// ConvException catches cases where the user
		// enters, for instance, "1.0", "1f", "20ul",
		// and other such representations that suggest
		// the number is not a regular 32-bit integer.
		if (isNumeric(guess)) {
			try {
				guessed_number = to!int(guess);
			} catch (ConvException e) {
				// stderr.writeln(e.msg);
				writeln("Please enter just an integer!");

				// Skip to the next iteration of the loop
				// to prevent the default value for guessed_number
				// being used below.
				continue;
			}
		} else {
			writeln("Please enter a number!");
			continue;
		}

		writeln("You guessed: ", guessed_number);

		if (guessed_number == secret_number) {
			writeln("You win!");
			break;
		} else if (guessed_number < secret_number) {
			writeln("Too small!");
		} else if (guessed_number > secret_number) {
			writeln("Too big!");
		} else {
			writeln("Hmm...");
		}

		user_turns++;
	}
}
