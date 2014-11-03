signature REGEX =
sig
  structure S : SEQUENCE

  type store
  type regex

  (* Used to look up values in the store. *)
  val retrieve : store -> int -> string option
  val retrieveAll : store -> string S.seq

  (* Used to construct regexes. Star and Question can be constructed using
   * or, cons, and plus. *)
  val cons : regex * regex -> regex
  val or : regex * regex -> regex
  val plus : regex -> regex
  val star : regex -> regex
  val maybe : regex -> regex

  (* Used to construct character literals and character classes. *)
  val chr : char -> regex
  val range : char * char -> regex
  val class : (char -> bool) -> regex
  val str : string -> regex

  (* Used to record the string matched by some regex. *)
  val track : regex -> regex

  (* Matches the empty string. *)
  val nothing : regex
  (* Matches any character except newline. *)
  val dot : regex

  (* Match the string against the regex. Note that the regex must match the
   * entire string for the match to be successful.
   * Evaluates to NONE if no match was found and SOME store otherwise where
   * store contains strings matched by tracked sub-regexes. *)
  val match : regex -> string -> store option
end
