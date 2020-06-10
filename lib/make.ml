module Player (World : sig
  val spawn_room : Player.t Ecs.Typed.t option -> Room.t Ecs.Typed.t

  val kills : (Ecs.Entity.t, int) Dict.t

  val deaths : (Ecs.Entity.t, int) Dict.t
end) =
struct
  open Base
  open Ecs.Infix
  include Player

  let make_new_player ~name ~client =
    let room = World.spawn_room None in
    {
      name;
      room;
      health = 100;
      loaded_ammo = 6;
      stored_ammo = 12;
      target = None;
      inventory = Dict.create (module Ecs.Entity);
      client;
    }

  let new_player client current_players =
    let name = new_name current_players in
    make_new_player ~name ~client

  let unaim_aiming_at target_player =
    Ecs.map Components.player ~f:(fun player ->
        match player.target with
        | Some target_player' when Ecs.Typed.(target_player = target_player') ->
            { player with target = None }
        | Some _ -> player
        | None -> player)

  let die ~source ~target =
    let old_player = !!target in
    let player' = make_new_player ~name:!!target.name ~client:!!target.client in
    unaim_aiming_at target;
    target =: player';
    let%lwt () = send target "You died! :-(" in
    List.init old_player.stored_ammo ~f:(fun _ ->
        ignore
        @@ Ecs.Typed.make Components.item (Item.make Bullet old_player.room))
    |> List.iter ~f:Fn.id;
    Dict.incr World.kills (Ecs.Typed.entity source);
    Dict.incr World.deaths (Ecs.Typed.entity target);
    Lwt.return_unit

  let damage ~source ~target amount =
    let player' = { !!target with health = !!target.health - amount } in
    target =: player';
    if !!target.health <= 0 then
      let%lwt () = die ~source ~target in
      true |> Lwt.return
    else false |> Lwt.return

  let fire source =
    match !!source.target with
    | None -> send source "You aren't aiming at anyone!"
    | Some target ->
        if !!source.loaded_ammo < 1 then send source "Your gun isn't loaded!"
        else (
          decr_ammo source;
          let%lwt () =
            send source (Printf.sprintf "You shot %s!" !!target.name)
          in
          let target_message =
            Printf.sprintf "You were shot by %s!" !!source.name
          in
          let%lwt () = send target target_message in
          let%lwt killed = damage ~source ~target 20 in
          if killed then send source "You killed them!" else Lwt.return_unit )

  let scoreboard player =
    let open Dict.Infix in
    let players = Ecs.selecti Components.player in
    let get_kills player = Option.value ~default:0 World.kills.@?[player] in

    let get_deaths player = Option.value ~default:0 World.deaths.@?[player] in
    let compare (p1, _) (p2, _) =
      let kills1, kills2 = (get_kills p1, get_kills p2) in
      let deaths1, deaths2 = (get_deaths p1, get_deaths p2) in
      if kills1 = kills2 then Int.compare deaths1 deaths2
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
