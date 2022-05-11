open! Core
open Input
module InputInt = InputInt

let get_ordering x y = Ordering.of_int @@ Int.compare x y

let ask_for_number answer total_turns () =
  let rec go turns_left =
    if turns_left = 0
    then print_endline "You lose!"
    else (
      Stdio.printf "You have %d turn(s) left.\n" turns_left;
      if turns_left = 1 then Stdio.print_endline "Uh oh! Make it count!!";
      let number =
        InputInt.prompt "Please enter your guess: " (fun x -> 1 <= x && x <= 100) ()
      in
      Stdio.printf "You guessed %d.\n" number;
      match get_ordering number answer with
      | Less ->
        Stdio.print_endline "Too low!";
        go (turns_left - 1)
      | Greater ->
        Stdio.print_endline "Too high!";
        go (turns_left - 1)
      | Equal -> Stdio.print_endline "You win!")
  in
  go total_turns
;;

let rec run_game total_turns () =
  Stdio.print_endline
    "I'm thinking of a number between 1 and 100! Can you guess which one?";
  let random_number = Random.int_incl 1 100 in
  ask_for_number random_number total_turns ();
  let continue = InputBool.yes_or_no "Do you want to play again?" () in
  if continue
  then (
    Stdio.print_endline "Okay, give me a moment to think of a new number!";
    run_game total_turns ())
  else Stdio.print_endline "Okay, thank you for playing!"
;;
