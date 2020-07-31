type t = {
  name: string;
  description: string;
  north: t Ecs.Typed.t option;
  south: t Ecs.Typed.t option;
  west: t Ecs.Typed.t option;
  east: t Ecs.Typed.t option;
}
