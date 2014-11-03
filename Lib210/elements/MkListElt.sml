functor MkListElt (structure Elt : ELEMENT) =
struct
  structure RegEx = Elt.RegEx
  open RegEx
  infix 5 or
  infix 5 cons

  exception NoCompare
  exception NoHash
  exception ParseErr

  val default = []
  type t = int list
  val equal : (int list * int list) -> bool = op=
  fun compare _ = raise NoCompare
  fun hash _ = raise NoHash
  fun toString xs =
      "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

  val regex = 
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
    in
      chr #"[" cons
      maybe (wrap Elt.regex cons star (chr #"," cons wrap Elt.regex)) cons
      chr #"]"
    end

  fun fromString s =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
      val tracker =
        chr #"[" cons
        maybe (wrap (track Elt.regex) cons
               star (chr #"," cons wrap (track Elt.regex))) cons
        chr #"]"
    in
      case match tracker s of
        NONE => raise ParseErr
      | SOME st => ((List.map Elt.fromString) o S.toList o retrieveAll) st
    end
end
