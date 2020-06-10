module Player (World : sig
  val spawn_room : Player.t Ecs.Typed.t option -> Room.t Ecs.Typed.t
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

  let die player =
    let old_player = !!player in
    let player' = make_new_player ~name:!!player.name ~client:!!player.client in
    unaim_aiming_at player;
    player =: player';
    let%lwt () = send player "You died! :-(" in
    List.init old_player.stored_ammo ~f:(fun _ ->
        ignore
        @@ Ecs.Typed.make Components.item (Item.make Bullet old_player.room))
    |> List.iter ~f:Fn.id;
    Lwt.return_unit

  let damage player amount =
    let player' = { !!player with health = !!player.health - amount } in
    player =: player';
    if !!player.health <= 0 then die player else Lwt.return_unit

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
            Printf.sprintf "You were shot by %s!" !!target.name
          in
          let%lwt () = send target target_message in
          damage target 20 )
end
