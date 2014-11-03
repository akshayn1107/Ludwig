functor MkSeqElt (structure Elt : ELEMENT
                  structure Seq : SEQUENCE) : ELEMENT =
struct
  structure RegEx = Elt.RegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = Elt.t Seq.seq

  exception ParseErr

  val default : t = Seq.empty ()
  val equal = Seq.equal Elt.equal
  val compare = Seq.collate Elt.compare
  fun toString s = Seq.toString Elt.toString s

  fun hash s =
      let
        fun rehash (h, e) =
            Word.+ (Word.fromInt (Elt.hash e),
                    Word.* (h, 0wx17))
      in Word.toIntX (Seq.iter rehash 0wx50356BCB s)
      end

  val regex =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
    in
      chr #"<" cons
      maybe (wrap Elt.regex cons star (chr #"," cons wrap Elt.regex)) cons
      chr #">"
    end

  fun fromString s =
    let
      fun wrap r = star (chr #" ") cons r cons star (chr #" ")
      val tracker =
        chr #"<" cons
        maybe (wrap (track Elt.regex) cons
               star (chr #"," cons wrap (track Elt.regex))) cons
        chr #">"
    in
      case match tracker s of
        NONE => raise ParseErr
      | SOME st => ((Seq.map Elt.fromString) o Seq.fromList o S.toList o 
                    retrieveAll) st
    end
end
