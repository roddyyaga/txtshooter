open Base
open React

let send_event, do_send = E.create ()

let send client message = do_send (client, message)

let sender =
  E.map
    (fun (client, message) ->
      message |> String.split_lines |> Lwt_list.iter_s (Ws.Client.send client))
    send_event
