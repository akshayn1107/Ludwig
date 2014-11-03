functor MkRegEx (structure S : SEQUENCE) : REGEX =
struct
  structure S = S

  open S

  (* Represent a regular expression:
   *   PLUS P represents /P+/
   *   CONS (P,Q) represents /PQ/
   *   EITHER (P,Q) represents /P|Q/
   *   CHAR x represents /x/ where x is a character class or character
   *   END represents the end of the regex
   *
   *   /P?Q/ can be represented with EITHER (Q, CONS (P,Q))
   *   /P*Q/ can be represented with EITHER (Q, CONS (PLUS P, Q))
   *)
  datatype regexI = PLUS of regexI | CONS of (regexI * regexI) |
                    EITHER of (regexI * regexI) | CHAR of (char -> bool) | END |
                    TRACK of (regexI)
  type regex = regexI

  fun cons (END, r) = r
    | cons (l, END) = l
    | cons (l, r) = CONS (l,r)
  fun or (l,r) = EITHER (l,r)
  fun plus r = PLUS r
  fun star r = EITHER (END, PLUS r)
  fun maybe r = EITHER (END, r)
  fun chr c = CHAR (fn x => x = c)
  fun range (c1,c2) = CHAR (fn x => x >= c1 andalso x <= c2)
  fun class f = CHAR f
  fun str s = iter cons END (map chr (% (String.explode s)))
  fun track (r) = TRACK (r)

  val nothing = END
  val dot = CHAR (fn x => x <> #"\n")

  type storeI = string seq
  type store = storeI
  datatype recordstate = OPEN | CLOSED
  type recorder = (recordstate * char list list)

  type state = regex * recorder

  exception InvalidState

  fun record (c : char) (r : regex, re : recorder) =
    case re of
      (CLOSED, l) => (r, (OPEN, [c]::l))
    | (OPEN, []) => raise InvalidState
    | (OPEN, x::xs) => (r, (OPEN, (c::x)::xs))

  fun close (re : recorder) : recorder =
    (CLOSED, #2 re)

  fun retrieveAll (s : store) : string seq = s

  fun retrieve (s : store) (ind : int) =
    SOME (nth s ind) handle Range => NONE

  fun step (c : char) (conser : state -> state) (ender : recorder -> state seq)
           (r : regex, re : recorder) : state seq =
    let
      (* Casing here is not necessary, but may marginally speed things up. *)
      fun cns y (x, re) =
        case (x, y) of 
          (END, y) => (y, re)
        | (x, END) => (x, re)
        | (x, y) => (CONS (x,y), re)

      fun trk (x, re) = (TRACK x, re)
    in
      case r of
          END => ender re
        | CHAR f =>
            if   f c
            then singleton (conser (END, re))
            else empty ()
        | PLUS r =>
            append (step c conser ender (r,re),
                    step c (conser o (cns (PLUS r))) ender (r,re))
        | EITHER (l,r) =>
            append (step c conser ender (l,re),
                    step c conser ender (r,re))
        | CONS (l,r) =>
            step c (conser o cns r)
                   (fn re' => step c conser ender (r,re')) (l,re)
        | TRACK r =>
            step c (conser o trk o record c)
                   (ender o close) (r,re)
    end

  fun match (r : regex) (s : string) : store option =
    let
      fun combine (rseq : (regex * recorder) seq, c) : (regex * recorder) seq =
        flatten (map (step c (fn x => x) (fn _ => empty ())) rseq)

      fun ended (re : regex) : bool =
        case re of
          END => true
        | CHAR _ => false
        | PLUS r => ended r
        | EITHER (l,r) => ended l orelse ended r
        | CONS (l,r) => ended l andalso ended r
        | TRACK r => ended r

      fun findEnded (rseq : state seq) : recorder option =
        if length rseq = 0 then NONE else
        let
          val (r, re) = nth rseq 0
        in
          if   ended r
          then SOME re
          else findEnded (drop (rseq,1))
        end

      fun getStore (rseq : (regex * recorder) seq) : store option =
        let
          val lToStr = String.implode o List.rev
          val lToSeq = % o List.rev
        in
          Option.map (map lToStr o lToSeq o #2) (findEnded rseq)
        end
    in
      getStore
        (iter
          combine
          (singleton (r, (CLOSED, [])))
          (% (String.explode s)))
    end
end
