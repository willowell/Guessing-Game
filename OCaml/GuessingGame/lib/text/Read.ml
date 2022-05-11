exception Read_failure of string

module type Read = sig
  type t

  val read : string -> t option
  val read_exn : string -> t
end

module ReadBool = struct
  type t = bool

  let read = function
    | "True" | "true" | "t" -> Some true
    | "False" | "false" | "f" -> Some false
    | _ -> None
  ;;

  let read_exn = function
    | "True" | "true" | "t" -> true
    | "False" | "false" | "f" -> false
    | _ -> raise (Read_failure "Could not read string as bool")
  ;;
end

let module_read_bool = (module ReadBool : Read with type t = bool)

module ReadInt = struct
  type t = int

  let read = int_of_string_opt
  let read_exn = int_of_string
end

let module_read_int = (module ReadInt : Read with type t = int)

module ReadFloat = struct
  type t = float

  let read = float_of_string_opt
  let read_exn = float_of_string
end

let module_read_float = (module ReadFloat : Read with type t = float)
