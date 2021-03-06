open Base
open Ecs.Infix
include Player0

let name_chars = "abcdefghijklmnopqrstuvwxyz" |> String.to_list

let possible_names =
  let ( * ) = List.cartesian_product in
  name_chars * name_chars * name_chars
  |> List.map ~f:(fun ((c1, c2), c3) -> [ c1; c2; c3 ] |> String.of_char_list)
  |> List.permute

let new_name players =
  let current_names = players |> List.map ~f:(fun p -> p.name) in
  List.find_exn
    ~f:(fun name -> not (List.mem current_names name ~equal:String.equal))
    possible_names

let send player message =
  let client = Ecs.(get_exn (Typed.entity player) Components.client) in
  Network.send client message

let look player =
  let message = !!player.room |> Room.look !!player in
  send player message

let look_in direction player =
  let message =
    match Room.in_direction direction !!(!!player.room) with
    | Error () -> Printf.sprintf "'%s' not recognised as a direction" direction
    | Ok None -> "There's nothing in that direction!"
    | Ok (Some room) -> Room.look !!player room
  in
  send player message

let decr_ammo player =
  player =: { !!player with loaded_ammo = !!player.loaded_ammo - 1 }

let find_by_name name =
  let players = Ecs.Typed.select Components.player in
  List.find players ~f:(fun p -> String.(!!p.name = name))

let aim source name =
  let target = find_by_name name in
  let message =
    let possible_player_target =
      match target with
      | Some target -> (
          match
            Room.reachable ~source:!!source.room ~destination:!!target.room
          with
          | true -> (
              let open Ecs.Infix in
              match target >>? Components.targetable with
              | Some _ -> Ok target
              | None -> Error "That player can't be targeted!" )
          | false -> Error (Printf.sprintf "%s is not in range!" !!target.name)
          )
      | None -> Error "There is no player with that name..."
    in

    match possible_player_target with
    | Ok target ->
        source
        =: { !!source with target = Some (target >>! Components.targetable) };
        Printf.sprintf "You aim your gun at %s." !!target.name
    | Error message -> (
        let item_and_targetable =
          !!source.room |> Room.contents
          |> List.filter_map ~f:(fun item ->
                 if String.(!!item.Item0.name = name) then
                   match item >>? Components.targetable with
                   | Some targetable -> Some (item, targetable)
                   | None -> None
                 else None)
          |> List.hd
        in
        match item_and_targetable with
        | Some (item, targetable) ->
            source =: { !!source with target = Some targetable };
            Printf.sprintf "You aim your gun at the %s." !!item.Item0.name
        | None -> message )
  in
  send source message

let examine_self source =
  Printf.sprintf
    "You are %s. You have %d health. You have %d bullets in your gun and %d \
     stored."
    !!source.name !!source.health !!source.loaded_ammo !!source.stored_ammo
  |> send source

let unknown_command source message =
  Printf.sprintf "Unknown command '%s'" message |> send source

let unaim_if_out_of_range player =
  let target_player =
    match !!player.target with
    | Some target -> Ecs.Infix.(target >>? Components.player)
    | None -> None
  in
  match target_player with
  | Some target ->
      if not (Room.reachable ~source:!!player.room ~destination:!!target.room)
      then (
        player =: { !!player with target = None };
        send player (Printf.sprintf "You lost track of %s!" !!target.name) )
  | None -> ()

let move player direction =
  let room = !!(!!player.room) in
  let target =
    match direction with
    | "north" -> Ok room.north
    | "south" -> Ok room.south
    | "west" -> Ok room.west
    | "east" -> Ok room.east
    | _other -> Error ()
  in
  match target with
  | Error () -> send player "Unknown direction"
  | Ok None -> send player "There is nothing in that direction..."
  | Ok (Some target_room) ->
      player =: { !!player with room = target_room };
      send player (Printf.sprintf "You went %s." (String.capitalize direction));
      Ecs.Typed.select Components.player |> List.iter ~f:unaim_if_out_of_range;
      look player

let reload player =
  match (!!player.loaded_ammo < 6, !!player.stored_ammo > 0) with
  | true, true ->
      let amount_to_load =
        Int.min (6 - !!player.loaded_ammo) !!player.stored_ammo
      in
      let player' =
        {
          !!player with
          loaded_ammo = !!player.loaded_ammo + amount_to_load;
          stored_ammo = !!player.stored_ammo - amount_to_load;
        }
      in
      player =: player';
      send player "You reloaded."
  | true, false -> send player "You don't have any ammunition to reload!"
  | false, _ -> send player "Your gun is already fully loaded."

let say player speech =
  let message = Printf.sprintf "%s says \"%s\"" !!player.name speech in
  send player (Printf.sprintf "You said \"%s\"" speech);
  let players = Ecs.Typed.select Components.player in
  List.iter
    ~f:(fun player' ->
      if not Ecs.Typed.(player' = player) then send player' message)
    players

let help player =
  {|Common commands:
  look - view your surroundings
  look [direction] - view the location to the North/South/East/West
  examine self - see information about yourself
  go [direction] - move to an adjacent location
  aim [player name] - set your sights on a player
  fire - shoot at the player you are aiming at
  reload - reload your gun
  say [message] - say something to other players
  scores - view the leaderboard
  map - view a map
  help - display this
  help 2 - view further help

You can shoot players in the the same room as you or adjacent ones.|}
  |> send player

let map player =
  {|T - T - T
|       |
T - F - T
|   |   |
T - C - T
|   |   |
T - O - T
|       |
T - T - T

T = twisty passage
F = fountain room
C = central room
O = other (non-fountain) room|}
  |> send player

let help2 player =
  {| Some commands have abbreviations:
  'l' and 'x' for look/examine|}
  |> send player
