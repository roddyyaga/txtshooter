open Base

type t = { rooms: Room.t list; players: (Ws.Client.Id.t, Player.t) Dict.t }

let fountain_room = Room.make "the fountain room" "A room with a fountain."

let other_room = Room.make "the non-fountain room" "A room without a fountain."

let central_room =
  Room.make "the central room"
    "A room with no interesting features other than a vague aura of centrality."

let twisty_passage_A =
  Room.make "a twizty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_B =
  Room.make "a twisty pasage" "A maze of twisty passages, mostly alike."

let twisty_passage_C =
  Room.make "a twisty paszage" "A maze of twisty passages, mostly alike."

let twisty_passage_D =
  Room.make "a twisty passege" "A maze of twisty passages, mostly alike."

let twisty_passage_E =
  Room.make "a tvisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_1 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_2 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_3 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_4 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_5 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_6 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_7 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let twisty_passage_8 =
  Room.make "a twisty passage" "A maze of twisty passages, mostly alike."

let () =
  Room.join twisty_passage_A twisty_passage_1 West_east;
  Room.join twisty_passage_1 twisty_passage_2 West_east;
  Room.join twisty_passage_A twisty_passage_3 North_south;
  Room.join twisty_passage_2 twisty_passage_B North_south;
  Room.join twisty_passage_3 fountain_room West_east;
  Room.join fountain_room twisty_passage_B West_east;
  Room.join twisty_passage_3 twisty_passage_C North_south;
  Room.join fountain_room central_room North_south;
  Room.join twisty_passage_B twisty_passage_4 North_south;
  Room.join twisty_passage_C central_room West_east;
  Room.join central_room twisty_passage_4 West_east;
  Room.join twisty_passage_C twisty_passage_5 North_south;
  Room.join central_room other_room North_south;
  Room.join twisty_passage_4 twisty_passage_D North_south;
  Room.join twisty_passage_5 other_room West_east;
  Room.join other_room twisty_passage_D West_east;
  Room.join twisty_passage_5 twisty_passage_E North_south;
  Room.join twisty_passage_D twisty_passage_7 North_south;
  Room.join twisty_passage_E twisty_passage_6 West_east;
  Room.join twisty_passage_6 twisty_passage_7 West_east

let () = ignore (Item.make Barrel fountain_room)

let distribute_dropped_bullets () =
  let players = Ecs.Typed.select Components.player in
  Ecs.filter_mapi Components.item ~f:(fun entity ({ Item0.name; _ } as item) ->
      let keep = Some item in
      let destroy = None in
      match name with
      | "bullet" -> (
          let open Ecs.Infix in
          let room = entity >! Components.location in
          let players_in_room =
            List.filter players ~f:(fun player ->
                Ecs.Typed.(!!player.room = room))
          in
          let player_to_give = List.random_element players_in_room in
          match player_to_give with
          | Some player ->
              let player' =
                Player.{ !!player with stored_ammo = !!player.stored_ammo + 1 }
              in
              (* TODO - clean up? *)
              Player.send player "You picked up a bullet.";
              player =: player';
              destroy
          | None -> keep )
      | _ -> keep)

let find_player target_client =
  let players = Ecs.Typed.select Components.player in
  List.find_exn players ~f:(fun player ->
      let open Ws.Client.Id in
      let client = Ecs.(get_exn (Typed.entity player) Components.client) in
      Ws.Client.id client = Ws.Client.id target_client)

let remove_player target_client =
  Ecs.filteri Components.player ~f:(fun entity _player ->
      let client = Ecs.get_exn entity Components.client in
      let open Ws.Client.Id in
      not (Ws.Client.id client = Ws.Client.id target_client))

let spawn_room _player_opt =
  List.random_element_exn [ fountain_room; other_room ]

let kills : (Ecs.Entity.t, int) Dict.t = Dict.create (module Ecs.Entity)

let deaths : (Ecs.Entity.t, int) Dict.t = Dict.create (module Ecs.Entity)
