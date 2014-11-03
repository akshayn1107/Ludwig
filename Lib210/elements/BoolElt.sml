structure BoolElt : ELEMENT =
struct
  structure RegEx = EltRegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = Bool.bool

  exception NoCompare
  exception NoHash
  exception ParseErr

  val default = true
  val equal : t * t -> bool = op=
  fun compare _ = raise NoCompare
  fun hash _ = raise NoHash
  val toString = Bool.toString
  fun fromString s = valOf (Bool.fromString s) handle Option => raise ParseErr

  val regex = str "true" or str "false"
end
