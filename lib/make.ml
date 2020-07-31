module Player (World : sig
  val spawn_room : Player.t Ecs.Typed.t option -> Room.t Ecs.Typed.t

  val kills : (Ecs.Entity.t, int) Dict.t

  val deaths : (Ecs.Entity.t, int) Dict.t
end) =
struct
  open Base
  open Ecs.Infix
  include Player

  let make_new_player ~name =
    let room = World.spawn_room None in
    {
      name;
      room;
      health = 100;
      loaded_ammo = 6;
      stored_ammo = 12;
      target = None;
      inventory = Dict.create (module Ecs.Entity);
    }

  let new_player current_players =
    let name = new_name current_players in
    make_new_player ~name

  let unaim_aiming_at target_player =
    Ecs.Typed.map Components.player ~f:(fun source_player ->
        let updated =
          let ( let* ) = Option.( >>= ) in
          let* source_target = !!source_player.target in
          let* source_target_player = source_target >>? Components.player in
          if Ecs.Typed.(target_player = source_target_player) then (
            send source_player "Your target ran out of your sights!";
            Some { !!source_player with target = None } )
          else None
        in
        match updated with Some update -> update | None -> !!source_player)

  let die ~source ~target =
    let old_player = !!target in
    let player' = make_new_player ~name:!!target.name in
    unaim_aiming_at target;
    target =: player';
    send target "You died! :-(";
    List.init old_player.stored_ammo ~f:(fun _ ->
        ignore @@ Item.make Bullet old_player.room)
    |> List.iter ~f:Fn.id;
    Dict.incr World.kills (Ecs.Typed.entity source);
    Dict.incr World.deaths (Ecs.Typed.entity target);
    look target

  let damage ~source ~target amount =
    let player' = { !!target with health = !!target.health - amount } in
    target =: player';
    if !!target.health <= 0 then (
      die ~source ~target;
      true )
    else false

  let shoot_event, do_shoot = React.E.create ()

  let shooting_handler = React.E.map (Shooting.handle damage) shoot_event

  let fire source =
    match !!source.target with
    | None -> send source "You aren't aiming at anyone!"
    | Some target -> do_shoot (source, target)

  let scoreboard player =
    let open Dict.Infix in
    let players = Ecs.selecti Components.player in
    let get_kills player = Option.value ~default:0 World.kills.@?[player] in

    let get_deaths player = Option.value ~default:0 World.deaths.@?[player] in
    let compare (p1, _) (p2, _) =
      let kills1, kills2 = (get_kills p1, get_kills p2) in
      let deaths1, deaths2 = (get_deaths p1, get_deaths p2) in
      if Int.(kills1 = kills2) then Int.compare deaths1 deaths2
      else Int.compare kills2 kills1
      (* Sort in descending order *)
    in
    let lines =
      List.map (List.sort players ~compare) ~f:(fun (entity, player) ->
          let kills, deaths = (get_kills entity, get_deaths entity) in
          Printf.sprintf "%4s %5d %6d" player.name kills deaths)
    in
    let message = "Name Kills Deaths\n" ^ String.concat ~sep:"\n" lines in
    Player.send player message
end
