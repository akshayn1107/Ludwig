signature ELEMENT =
sig
  structure RegEx : REGEX
  type t
  val default : t
  val equal : t * t -> bool
  val compare : t * t -> order
  val hash : t -> int
  val toString : t -> string
  val regex : RegEx.regex
  val fromString : string -> t
end
