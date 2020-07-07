import prompts, { PromptObject, Answers } from 'prompts'
import random from 'random'

const numberPrompt: PromptObject<string> = {
  type: 'number',
  name: 'guess',
  message: 'Please enter your guess: ',
  initial: 50,
  min: 1,
  max: 100
}

const playAgainPrompt: PromptObject<string> = {
  type: 'confirm',
  name: 'again',
  message: 'Do you want to play again?',
  initial: true
}

async function runGame (answer: number, totalTurns: number): Promise<void> {
  let turnsLeft: number = totalTurns

  console.log("I'm thinking of a number between 1 and 100! Can you guess which one?")

  while (true) {
    if (turnsLeft === 0) {
      console.log('You lose!')
      break
    }
    console.log(`You have ${turnsLeft} turn(s) left.`)
    if (turnsLeft === 1) {
      console.log('Uh oh! Make it count!!')
    }

    const { guess }: Answers<string> = await prompts(numberPrompt)

    console.log(`You guessed ${guess as number}.`)

    if (guess as number < answer) {
      console.log('Too low!')
      turnsLeft--
    } else if (guess as number > answer) {
      console.log('Too high!')
      turnsLeft--
    } else {
      console.log('You won!')
      break
    }
  }
}

async function main (): Promise<void> {
  const turnLimit: number = 10

  while (true) {
    await runGame(random.int(1, 100), turnLimit)

    const { again }: Answers<string> = await prompts(playAgainPrompt)

    if (again as boolean) {
      console.log('Okay, give me a moment to think of a new number!')
    } else {
      console.log('Okay, thank you for playing!')
      break
    }
  }
}

// eslint-disable-next-line @typescript-eslint/no-floating-promises
main()
