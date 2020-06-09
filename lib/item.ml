type t = { name: string; description: string option; room: Room0.t Ecs.Typed.t }

type item = Bullet

let make item room =
  let name, description =
    match item with Bullet -> ("bullet", Some "a bullet")
  in
  { name; description; room }
