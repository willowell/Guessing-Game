using System;
using System.Linq;

using Maybe;

namespace IO {
	struct Input { 
		public static Maybe<string> ReadLine() {
			try {
				return Maybe<string>.Some(Console.ReadLine());
			} catch (Exception) {
				return Maybe<string>.None;
			}
		}

		public static Maybe<int> ReadInt(Maybe<string> arg) {
			try {
				return arg.Map(str => int.Parse(str));
			} catch (Exception) {
				return Maybe<int>.None;
			}
		}

		public static Maybe<string> PromptLine(string prompt) {
			Console.WriteLine(prompt);
			Console.Out.Flush();
			return ReadLine();
		}

		public static int InputInt(string prompt, Func<int, bool> validator) {
			bool validate(Maybe<int> mx) => mx.Match(
				x => validator(x),
				() => false
			);

			Maybe<int> userInput;

			for (
				userInput = ReadInt(PromptLine(prompt));
				!validate(userInput);
				userInput = ReadInt(PromptLine(prompt))
			) {
				Console.WriteLine("Invalid input. Please try again.");
			}

			return userInput.GetOrDefault(0);
		}

		public static string InputString(string prompt, Func<string, bool> validator) {
			bool validate(Maybe<string> mx) => mx.Match(
				x => validator(x),
				() => false
			);

			Maybe<string> userInput;

			for (
				userInput = PromptLine(prompt);
				!validate(userInput);
				userInput = PromptLine(prompt)
			) {
				Console.WriteLine("Invalid input. Please try again.");
			}

			return userInput.GetOrDefault(string.Empty);
		}

		public static bool YesOrNo(string prompt) {
			string[] answers = { "yes", "y", "no", "n" };

			var userInput = InputString(prompt, str => answers.Any(i => i.Equals(str)));

			return userInput == "yes" || userInput == "y";
		}
	}
}
