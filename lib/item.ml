let make item_tag room =
  let item = Ecs.Typed.make Components.item (Item0.make item_tag) in
  Ecs.set (Ecs.Typed.entity item) Components.location room;
  let () =
    match item_tag with
    | Barrel -> Ecs.set (Ecs.Typed.entity item) Components.targetable ()
    | _ -> ()
  in
  item
