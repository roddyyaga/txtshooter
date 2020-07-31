open Base

module Entity : sig
  type t

  val empty : unit -> t

  val equal : t -> t -> bool

  val ( = ) : t -> t -> bool

  include Hashtbl.Key.S with type t := t
end

module Component : sig
  module type S = sig
    type t
  end

  type 'a t = (module S with type t = 'a)
end

type 'a component

val component : 'a Component.t -> 'a component

val get : Entity.t -> 'a component -> 'a option

val get_exn : Entity.t -> 'a component -> 'a

val has : Entity.t -> 'a component -> bool

val set : Entity.t -> 'a component -> 'a -> unit

val create : 'a component -> 'a -> Entity.t

val select : 'a component -> 'a list

val selecti : 'a component -> (Entity.t * 'a) list

val select2 : 'a component -> 'b component -> ('a * 'b) list

val select2i : 'a component -> 'b component -> (Entity.t * ('a * 'b)) list

val filter : 'a component -> f:('a -> bool) -> unit

val filteri : 'a component -> f:(Entity.t -> 'a -> bool) -> unit

val map : 'a component -> f:('a -> 'a) -> unit

val mapi : 'a component -> f:(Entity.t -> 'a -> 'a) -> unit

val filter_map : 'a component -> f:('a -> 'a option) -> unit

val filter_mapi : 'a component -> f:(Entity.t -> 'a -> 'a option) -> unit

module Typed : sig
  type 'a t

  val entity : 'a t -> Entity.t

  val ( = ) : 'a t -> 'a t -> bool

  val make : 'a component -> 'a -> 'a t

  val of_entity : Entity.t -> 'a component -> 'a t option

  val coerce : 'a t -> 'b component -> 'b t option

  val of_entity_exn : Entity.t -> 'a component -> 'a t

  val coerce_exn : 'a t -> 'b component -> 'b t

  val monad_map : ('a -> 'a) -> 'a t -> 'a t

  val iter : 'a t -> f:('a -> unit) -> unit

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val has : 'a t -> 'b component -> bool

  val select : 'a component -> 'a t list

  val map : 'a component -> f:('a t -> 'a) -> unit
end

module Infix : sig
  val ( >? ) : Entity.t -> 'a component -> 'a option

  val ( >! ) : Entity.t -> 'a component -> 'a

  val ( !! ) : 'a Typed.t -> 'a

  val ( =: ) : 'a Typed.t -> 'a -> unit

  val ( >>? ) : 'a Typed.t -> 'b component -> 'b Typed.t option

  val ( >>! ) : 'a Typed.t -> 'b component -> 'b Typed.t
end

module Syntax : sig
  val ( let+ ) : 'a Typed.t -> ('a -> 'a) -> 'a Typed.t
end
