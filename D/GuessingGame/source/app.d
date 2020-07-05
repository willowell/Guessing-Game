import std.datetime;
import std.random;
import std.stdio;
import std.string;     // -> strip
import std.conv;       //-> to!()
import std.exception;  //-> The exceptions
import std.algorithm;
import std.array;
import optional;

Optional!string readLine() {
	string line;
	try {
		line = stdin.readln();
	} catch (StdioException e) {
		// An I/O error has occurred!
		stderr.writeln(e.msg);
		return no!string;
	} catch (Exception e) {
		// Some other error has occurred!
		stderr.writeln(e.msg);
		return no!string;
	}
	return some(line);
}

Optional!string promptLine(string prompt) {
	writeln(prompt);
	stdout.flush();
	return readLine();
}

Optional!U convert(U, T)(T arg) {
	import std.conv: to;
	scope(failure) return no!U;
	return some(arg.to!U);
}

Optional!T read(T)(Optional!string arg) {
	return arg.match!(
		(string s) => s.strip.convert!T,
		() => no!T
	);
}

auto input(T)(string prompt, bool delegate(T arg) validator) {
	T userInput;

	for (
		userInput = promptLine(prompt).strip().to!T();
		!validator(userInput);
		userInput = promptLine(prompt).strip().to!T()
	) {
		writeln("Invalid input. Please try again.");
	}

	return userInput;
}

void main() {

	//int x = input!int("Please enter a number in [1, 100]: ", (x) => 1 <= x && x <= 100);

	Optional!string strx = promptLine("Enter a number: ");
	Optional!int nx = read!int(strx);

	nx.match!((int x) => writeln("Got: ", x), () => writeln("Sorry, nothing..."));
}
