open Base

let list_with_and xs =
  match xs with
  | [] -> "nothing"
  | [ x ] -> Printf.sprintf "%s" x
  | x :: x' :: xs' -> String.concat ~sep:", " (x' :: xs') ^ " and " ^ x

module String_helpers = struct
  let are xs = match List.length xs with 0 | 1 -> "is" | _ -> "are"

  let a s =
    match s.[0] with 'a' | 'e' | 'i' | 'o' | 'u' -> "an " ^ s | _ -> "a " ^ s
end
