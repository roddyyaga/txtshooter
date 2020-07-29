type t = {
  name: string;
  room: Room0.t Ecs.Typed.t;
  health: int;
  loaded_ammo: int;
  stored_ammo: int;
  target: t Ecs.Typed.t option;
  inventory: (Ecs.Entity.t, int) Dict.t;
  client: Ws.Client.t;
}

let ( = ) p1 p2 = p1.name = p2.name
