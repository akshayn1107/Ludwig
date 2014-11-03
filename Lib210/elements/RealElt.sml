structure RealElt : ELEMENT =
struct
  structure RegEx = EltRegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = Real.real

  exception ParseErr

  val default = 0.0
  val equal = Real.==
  val compare = Real.compare
  val toString = Real.toString
  fun hash(v) = 
      let
        val {man=m, exp=_} = Real.toManExp v
        fun ihash i = Word.toIntX (Word.* (0wx50356BCB, Word.fromInt i))
      in ihash (round (m * 1000000000.0))
      end
  fun fromString s = valOf (Real.fromString s) handle Option => raise ParseErr

  val regex =
    let
      val digit = class Char.isDigit
      val afterPt = chr #"." cons plus digit
    in
      maybe (chr #"+" or chr #"~" or chr #"-") cons
      ((plus digit cons maybe afterPt) or afterPt)
    end  
end
