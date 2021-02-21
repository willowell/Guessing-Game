import Ask from "ask";
import * as ink from "ink";
import Random from "random";

const ask = new Ask();

const rand = new Random();

async function runGame(answer: number, totalTurns: number): Promise<void> {
  let turnsLeft: number = totalTurns;

  console.log([
    ink.colorize("<b>I'm thinking of a number between</b>"),
    ink.colorize("<cyan>1</cyan>"),
    ink.colorize("<b>and</b>"),
    ink.colorize("<b><cyan>100</cyan>!</b>"),
    ink.colorize("<b>Can you guess which one?</b>"),
  ].join(" "));

  while (true) {
    if (turnsLeft === 0) {
      console.log("You lose!");
      break;
    }

    console.log(
      `You have ${
        ink.colorize(`<b><cyan>${turnsLeft}</cyan></b>`)
      } turn(s) left.`,
    );

    if (turnsLeft === 1) {
      console.log(
        ink.colorize("<b><yellow>Uh oh! Make it count!!</yellow></b>"),
      );
    }

    const { guess } = await ask.number({
      name: "guess",
      message: "Please enter your guess:",
      min: 1,
      max: 100,
    });

    console.log(
      `You guessed ${
        ink.colorize(`<b><yellow>${guess as number}</yellow></b>`)
      }.`,
    );

    if (guess as number < answer) {
      console.log(
        ink.colorize("<b><red>Too low!</red></b>"),
      );

      turnsLeft--;

    } else if (guess as number > answer) {
      console.log(
        ink.colorize("<b><red>Too high!</red></b>"),
      );

      turnsLeft--;

    } else {
      console.log(
        ink.colorize("<b><cyan>You won!</cyan></b>"),
      );

      break;
    }
  }
}

export default async function main(args?: string[]): Promise<void> {
  const turnLimit: number = 10;

  while (true) {
    await runGame(rand.int(1, 101), turnLimit);

    const { again } = await ask.confirm({
      name: "again",
      message: "Do you want to play again?",
    });

    if (again as boolean) {
      console.log("Okay, give me a moment to think of a new number!");
    } else {
      console.log("Okay, thank you for playing!");
      break;
    }
  }
}