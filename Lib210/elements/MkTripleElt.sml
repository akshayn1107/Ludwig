functor MkTripleElt (structure EltA : ELEMENT
                     structure EltB : ELEMENT
                     structure EltC : ELEMENT
                     sharing EltA.RegEx = EltB.RegEx and
                             EltB.RegEx = EltC.RegEx) : ELEMENT =
struct
  structure RegEx = EltA.RegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = EltA.t * EltB.t * EltC.t

  exception NYI
  exception ParseErr

  val default = (EltA.default, EltB.default, EltC.default)

  fun equal ((xa,xb,xc),(ya,yb,yc)) =
      EltA.equal(xa,ya) andalso EltB.equal(xb,yb) andalso EltC.equal(xc,yc)

  fun compare ((xa,xb,xc),(ya,yb,yc)) =
      case EltA.compare (xa,ya)
        of EQUAL => (case EltB.compare (xb,yb)
                       of EQUAL => EltC.compare (xc,yc)
                        | ordb => ordb)
         | orda => orda

  fun hash (a,b,c) = raise NYI

  fun toString (a,b,c) = 
      "(" ^ EltA.toString a ^ "," ^
            EltB.toString b ^ "," ^
            EltC.toString c ^ ")"

  val regex =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
    in
      chr #"(" cons
      wrap EltA.regex cons chr #"," cons
      wrap EltB.regex cons chr #"," cons
      wrap EltC.regex cons
      chr #")"
    end

  fun fromString s =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
      val tracker =
        chr #"(" cons
        wrap (track EltA.regex) cons chr #"," cons
        wrap (track EltB.regex) cons chr #"," cons
        wrap (track EltC.regex) cons
        chr #")"
    in
      case match tracker s of
        NONE => raise ParseErr
      | SOME st =>
         (case (retrieve st 0, retrieve st 1, retrieve st 2) of
            ((NONE, _, _)|(_, NONE, _)|(_, _, NONE)) => raise ParseErr
          | (SOME a, SOME b, SOME c) =>
              (EltA.fromString a, EltB.fromString b, EltC.fromString c))
    end
end
