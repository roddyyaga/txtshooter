open Base
open Ecs.Infix
include Room0

let make name description =
  Ecs.Typed.make Components.room
    {
      name;
      description;
      north = None;
      south = None;
      west = None;
      east = None;
      contents = Dict.create (module Ecs.Entity);
    }

type axis = North_south | West_east

let join first_room second_room axis =
  match axis with
  | North_south ->
      first_room =: { !!first_room with south = Some second_room };
      second_room =: { !!second_room with north = Some first_room }
  | West_east ->
      first_room =: { !!first_room with east = Some second_room };
      second_room =: { !!second_room with west = Some first_room }

let look looking_player room =
  let direction_msg direction room_opt =
    match room_opt with
    | Some room -> Printf.sprintf "To the %s you see %s." direction !!room.name
    | None -> ""
  in
  let n_message = direction_msg "North" !!room.north in
  let s_message = direction_msg "South" !!room.south in
  let w_message = direction_msg "West" !!room.west in
  let e_message = direction_msg "East" !!room.east in

  let players =
    Ecs.select Components.player
    |> List.filter ~f:(fun player -> Ecs.Typed.(player.Player0.room = room))
  in

  let open Utils.String_helpers in
  let items_message =
    let items =
      Ecs.select Components.item
      |> List.filter ~f:(fun item -> Ecs.Typed.(item.Item.room = room))
    in
    match items with
    | [] -> ""
    | items ->
        let item_counts =
          let counts = Dict.create (module String) in
          items |> List.iter ~f:(fun item -> Dict.incr counts item.Item.name);
          counts
        in
        let numbered_item_string =
          Dict.to_alist item_counts
          |> List.map ~f:(fun (name, count) ->
                 if count > 1 then Printf.sprintf "%d %ss" count name
                 else a name)
          |> Utils.list_with_and
        in
        Printf.sprintf " You also see %s." numbered_item_string
  in

  let players_message =
    match players with
    | [] -> "There is no-one in this room."
    | players ->
        let names_string =
          players
          |> List.map ~f:(fun p ->
                 if Player0.(p = looking_player) then p.Player0.name
                 else Printf.sprintf "%s (you)" p.Player0.name)
          |> Utils.list_with_and
        in
        Printf.sprintf "In here %s %s." (are players) names_string
  in

  Printf.sprintf "%s.\n%s %s %s %s %s %s%s\n" !!room.name !!room.description
    n_message s_message w_message e_message players_message items_message
  |> String.capitalize

let in_direction direction room =
  match direction with
  | "north" -> Ok room.north
  | "south" -> Ok room.south
  | "west" -> Ok room.west
  | "east" -> Ok room.east
  | _ -> Error ()

let reachable ~source ~destination =
  let adjacent =
    List.filter_opt
      [ !!source.north; !!source.south; !!source.west; !!source.east ]
  in
  Ecs.Typed.(source = destination)
  || List.mem adjacent destination ~equal:Ecs.Typed.( = )
