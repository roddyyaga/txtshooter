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

val get_exn : Entity.t -> 'a component -> 'a

val set : Entity.t -> 'a component -> 'a -> unit

val create : 'a component -> 'a -> Entity.t

val select : 'a component -> 'a list

val selecti : 'a component -> (Entity.t * 'a) list

val select2 : 'a component -> 'b component -> ('a * 'b) list

val select2i : 'a component -> 'b component -> (Entity.t * ('a * 'b)) list

module Typed : sig
  type 'a t

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit

  val select : 'a component -> 'a t list

  val make : 'a component -> 'a -> 'a t

  val map : ('a -> 'a) -> 'a t -> 'a t
end

module Infix : sig
  val ( !! ) : 'a Typed.t -> 'a

  val ( =: ) : 'a Typed.t -> 'a -> unit
end

module Syntax : sig
  val ( let+ ) : 'a Typed.t -> ('a -> 'a) -> 'a Typed.t
end
