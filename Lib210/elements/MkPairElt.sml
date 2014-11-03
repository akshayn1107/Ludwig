functor MkPairElt (structure EltA : ELEMENT
                   structure EltB : ELEMENT
                   sharing EltA.RegEx = EltB.RegEx) : ELEMENT =
struct
  structure RegEx = EltA.RegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = EltA.t * EltB.t

  exception ParseErr

  val default = (EltA.default, EltB.default)

  fun equal ((xa,xb),(ya,yb)) =
      EltA.equal (xa,ya) andalso EltB.equal (xb,yb)

  fun compare ((xa,xb),(ya,yb)) =
      case EltA.compare (xa,ya)
        of EQUAL => EltB.compare (xb,yb)
         | ord => ord

  fun hash (a,b) =
      Word.toIntX (Word.* (Word.+ (Word.fromInt (EltA.hash a), 0wxB),
                     Word.* (Word.+ (Word.fromInt (EltB.hash b), 0wx17),
                             0wx50356BCB)))

  fun toString (a,b) =
      "(" ^ EltA.toString a ^ "," ^ EltB.toString b ^ ")"

  val regex =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
    in
      chr #"(" cons
      wrap EltA.regex cons chr #"," cons
      wrap EltB.regex cons
      chr #")"
    end

  fun fromString s =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
      val tracker =
        chr #"(" cons
        wrap (track (EltA.regex)) cons chr #"," cons
        wrap (track (EltB.regex)) cons
        chr #")"
    in
      case match tracker s of
        NONE => raise ParseErr
      | SOME st =>
         (case (retrieve st 0, retrieve st 1) of
            ((NONE, _)|(_, NONE)) => raise ParseErr
          | (SOME a, SOME b) => (EltA.fromString a, EltB.fromString b))
    end

end
