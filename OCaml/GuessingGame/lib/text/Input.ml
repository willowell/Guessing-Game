open Read

module Input (R : Read) = struct
  open R

  let input_line (msg : string) () : string option =
    Stdio.print_endline msg;
    flush stdout;
    try Some (read_line ()) with
    | End_of_file -> None
  ;;

  let input msg () = Option.bind (input_line msg ()) read

  let rec prompt msg validator () =
    let repeat () =
      Stdio.print_endline "Invalid input";
      prompt msg validator ()
    in
    let res = input msg () in
    match res with
    | Some x -> if validator x then x else repeat ()
    | None -> repeat ()
  ;;
end

module InputInt = Input (ReadInt)

module InputBool = struct
  include Input (ReadBool)

  let rec yes_or_no msg () : bool =
    let repeat () =
      print_endline "Invalid input.";
      yes_or_no msg ()
    in
    let str =
      match input_line (msg ^ " (y/n): ") () with
      | Some str -> str
      | None -> ""
    in
    match String.lowercase_ascii str with
    | "y" -> true
    | "n" -> false
    | _ -> repeat ()
  ;;
end
