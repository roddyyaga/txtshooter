open Base
open Dict.Infix

let component_maps = ref Hmap.empty

let next_id = ref 1

module Entity = struct
  type t = int

  let hash = Int.hash

  let compare = Int.compare

  let sexp_of_t = Int.sexp_of_t

  let empty () =
    let id = !next_id in
    next_id := id + 1;
    id

  let equal = Int.equal

  let ( = ) = Int.( = )
end

module Component = struct
  module type S = sig
    type t
  end

  type 'a t = (module S with type t = 'a)
end

type 'a component = (Entity.t, 'a) Dict.t Hmap.key

let component _m =
  let c = Hmap.Key.create () in
  component_maps := Hmap.add c (Dict.create (module Int)) !component_maps;
  c

let get_exn t component =
  let component_map = Hmap.get component !component_maps in
  component_map.@![t]

let set t component value =
  let component_map = Hmap.get component !component_maps in
  component_map.@[t] <- value

let create component value =
  let entity = Entity.empty () in
  set entity component value;
  entity

let select component = Hmap.get component !component_maps |> Dict.data

let selecti component = Hmap.get component !component_maps |> Dict.to_alist

let select2 c1 c2 =
  let c1s = Hmap.get c1 !component_maps in
  let c2s = Hmap.get c2 !component_maps in
  Dict.combine c1s c2s |> Dict.data

let select2i c1 c2 =
  let c1s = Hmap.get c1 !component_maps in
  let c2s = Hmap.get c2 !component_maps in
  Dict.combine c1s c2s |> Dict.to_alist

let filter component ~f =
  Hmap.get component !component_maps |> Dict.filter_inplace ~f

let map component ~f = Hmap.get component !component_maps |> Dict.map_inplace ~f

let mapi component ~f =
  Hmap.get component !component_maps
  |> Dict.mapi_inplace ~f:(fun ~key ~data -> f key data)

let filter_map component ~f =
  Hmap.get component !component_maps |> Dict.filter_map_inplace ~f

module Typed = struct
  type 'a t = 'a component * Entity.t

  let entity (_component, entity) = entity

  let ( = ) t t' = Entity.(entity t = entity t')

  let get (component, entity) = get_exn entity component

  let set (component, entity) value = set entity component value

  let make component value =
    let entity = create component value in
    (component, entity)

  let select component =
    List.map
      ~f:(fun (entity, _value) -> (component, entity))
      (selecti component)

  let map f (component, entity) = make component (f (get (component, entity)))
end

module Infix = struct
  let ( !! ) = Typed.get

  let ( =: ) = Typed.set
end

module Syntax = struct
  let ( let+ ) x f = Typed.map f x
end
