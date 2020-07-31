type t = { name: string; description: string option }

type item = Bullet | Barrel

let make item =
  let name, description =
    match item with
    | Bullet -> ("bullet", Some "a bullet")
    | Barrel -> ("barrel", Some "a barrel")
  in
  { name; description }
