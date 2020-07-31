open Base

let handle damage_player
    ((source : Player0.t Ecs.Typed.t), (target : Targetable.t Ecs.Typed.t)) =
  let open Ecs.Infix in
  if !!source.loaded_ammo < 1 then Player.send source "Your gun isn't loaded!"
  else
    match target >>? Components.player with
    | Some target_player ->
        Player.decr_ammo source;
        Player.send source (Printf.sprintf "You shot %s!" !!target_player.name);
        let target_message =
          Printf.sprintf "You were shot by %s!" !!source.name
        in
        Player.send target_player target_message;
        let killed = damage_player ~source ~target:target_player 20 in
        if killed then Player.send source "You killed them!"
    | None -> (
        match target >>? Components.item with
        | Some item ->
            Player.send source (Printf.sprintf "Shot the %s" !!item.Item0.name)
        | None -> Player.send source "Shooting that is not handled" )
