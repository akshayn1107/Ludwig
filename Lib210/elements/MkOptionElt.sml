functor MkOptionElt (structure Elt : ELEMENT) : ELEMENT =
struct
  structure RegEx = Elt.RegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = Elt.t option

  exception NoCompare
  exception ParseErr

  val default = NONE

  fun equal (NONE, NONE) = true
    | equal (SOME x, SOME y) = Elt.equal (x, y)
    | equal _ = false

  fun compare (NONE, NONE) = EQUAL
    | compare (NONE, SOME _) = LESS
    | compare (SOME _, NONE) = GREATER
    | compare (SOME x, SOME y) = Elt.compare (x, y)

  fun hash NONE = 0
    | hash (SOME x) = Elt.hash x

  fun toString NONE = "NONE"
    | toString (SOME x) = "SOME (" ^ Elt.toString x ^ ")"

  val regex = str "NONE" or (str "SOME" cons plus (chr #" ") cons Elt.regex)

  fun fromString s =
    let
      val tracker =
        str "NONE" or (str("SOME") cons plus(chr #" ") cons track(Elt.regex))
    in
      case match tracker s of
        NONE => raise ParseErr
      | SOME st => Option.map Elt.fromString (retrieve st 0)
    end
end
