module Position = struct
  type t = { x: float; y: float }
end

let position = Ecs.component (module Position)

module Name = struct
  type t = string
end

let name = Ecs.component (module Name)

let positions = Ecs.selecti position
