signature PARSER =
sig
  type out
  val parse : string -> out
  val parseFile : string -> out
end
