structure IntElt : ELEMENT =
struct
  structure RegEx = EltRegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = Int.int

  exception ParseErr

  val default = 0
  val equal = op=
  val compare = Int.compare
  fun hash i = Word.toIntX (Word.* (0wx50356BCB, Word.fromInt i))
  val toString = Int.toString

  fun fromString s = valOf (Int.fromString s) handle Option => raise ParseErr

  val regex = plus (class Char.isDigit)
end
