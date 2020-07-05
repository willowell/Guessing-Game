module input;

import std.algorithm;
import std.array;
import std.ascii;
import std.conv;
import std.exception;
import std.stdio;
import std.string;

import optional;

Optional!string readLine() {
    string line;
    try {
        stdin.flush();
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

public Optional!string promptLine(string prompt) {
    writeln(prompt);
    stdout.flush();
    return readLine();
}

Optional!U convert(U, T)(T arg) {
    import std.conv: to;
    scope(failure) return no!U;
    return some(arg.to!U);
}

public Optional!T read(T)(Optional!string arg) {
    return arg.match!(
    (string s) => s.strip.convert!T,
    () => no!T
    );
}

public auto input(T)(string prompt, bool delegate(T arg) validator) {
    bool validate(Optional!T arg) {
        return arg.match!(
        (T x) => validator(x),
        () => false
        );
    }

    Optional!T userInput;

    for (
        userInput = promptLine(prompt).read!T();
        !validate(userInput);
        userInput = promptLine(prompt).read!T()
    ) {
        writeln("Invalid input. Please try again.");
    }

    return userInput.front();
}

public auto inputInt(string prompt, bool delegate(int arg) validator) {
    return input!int(prompt, validator);
}

public auto inputChar(string prompt, bool delegate(char arg) validator) {
    return input!char(prompt, validator);
}

bool yesOrNo(string prompt) {
    char answer = inputChar(prompt, delegate(char arg) {
        arg = std.ascii.toUpper(arg);
        return (arg == 'Y' || arg == 'N');
    });

    return (answer == 'Y');
}