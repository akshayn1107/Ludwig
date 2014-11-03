structure StringElt : ELEMENT = 
struct
  structure RegEx = EltRegEx
  open RegEx
  infix 5 cons
  infix 5 or

  type t = String.string

  exception ParseErr

  val default = ""
  val equal : t * t -> bool = op=
  val compare = String.compare
  fun toString s = "\"" ^ String.toString s ^ "\""
  fun hash s = 
      let
        fun subs i = Word.fromInt (Char.ord (String.sub (s,i)))
        val c = Word.fromInt 65599
        fun hash'(i,h) = 
            if i < 0 then h else hash' (i-1, subs i + h * c)
      in Word.toIntX (hash'(String.size s -1, Word.fromInt 0))
      end

  val regex =
    let
      val noesc = class (fn x => x <> #"\"" andalso x <> #"\\")
      val escc = class (fn x => x = #"a" orelse x = #"b" orelse x = #"t"
                        orelse x = #"n"  orelse x = #"v" orelse x = #"f"
                        orelse x = #"r" orelse x = #"\\" orelse x = #"\"")
      val esc = chr #"\\" cons escc
    in
      chr #"\"" cons star noesc cons star (esc cons star noesc) cons chr #"\""
    end

  fun fromString s =
    case match regex s of
      NONE => raise ParseErr
    | SOME _ =>
        valOf (String.fromString (String.substring (s, 1, String.size s - 2)))
        handle Option => raise ParseErr
end
