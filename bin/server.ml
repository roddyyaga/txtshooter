open Base
open Lib
module Player = Make.Player (World)

let int_id client = Ws.Client.(client |> id |> Id.to_int)

let events = Queue.create ()

let expand_abbreviations split_message =
  List.map split_message ~f:(function
    | "l" | "x" | "examine" -> "look"
    | other -> other)

let new_player client =
  let players = Ecs.select Components.player in
  let new_player_c = Player.new_player client players in
  Ecs.Typed.make Components.player new_player_c

let rec process_events events =
  let open Event in
  match Queue.dequeue events with
  | None -> Lwt.return_unit
  | Some (New_player client) ->
      let new_player = new_player client in
      let%lwt () = Player.look new_player in
      Player.send new_player "Enter 'help' for a list of commands."
  | Some (Command (client, message)) ->
      let message = String.lowercase message in
      let%lwt () =
        let player = World.find_player client in
        let open Player in
        match String.split ~on:' ' message |> expand_abbreviations with
        | [ "look" ] -> look player
        | [ "look"; "self" ] -> examine_self player
        | [ "look"; direction ] -> player |> look_in direction
        | [ "aim"; target ] -> aim player target
        | [ "fire" ] -> fire player
        | [ "go"; direction ] -> move player direction
        | [ "help" ] -> help player
        | [ "reload" ] -> reload player
        | _other -> unknown_command player message
      in
      process_events events

let rec game_loop events =
  let%lwt () = process_events events in
  let%lwt () = Lwt_main.yield () in
  game_loop events

let on_connect client =
  Stdio.print_endline "New player!";
  Queue.enqueue events (Event.New_player client) |> Lwt.return

let on_close client _message =
  Stdio.printf "Client %d connection closed\n%!" (int_id client) |> Lwt.return

let handle client message =
  Queue.enqueue events (Event.Command (client, message));
  Stdio.printf "Client %d: '%s'\n%!" (int_id client) message |> Lwt.return

let server = Ws.Server.create ~port:1337

let () = Stdio.print_endline "Game loop start"

let () = Lwt.async (fun () -> game_loop events)

let () = Stdio.print_endline "Game loop started"

let () = Lwt_main.run (Ws.Server.run ~on_connect server handle)
