import promptsync from 'prompt-sync'

import _ from "lodash"

const prompt = promptsync({})

async function main() {
  console.log("Hello!")

  let s = prompt("Give it to me straight: ")

  console.log(`"${s}"... I see. Thank you.`)

  let s = prompt("Give it to me straight again: ")

  console.log(`"${s}"... I see. Thank you.`)
}

main()