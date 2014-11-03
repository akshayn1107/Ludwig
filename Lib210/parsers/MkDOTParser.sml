(* There's a lot of dead code here that can be used to generate a heuristic
 * based on parsed cartesian coordinates. I might use it in the future or
 * remove it. Ignore it for now. -Naman *)
functor MkDOTParser (structure Seq : SEQUENCE
                     structure Vx : ELEMENT
                     structure Wt : ELEMENT
                     (* structure Pos : ELEMENT *)
                     sharing Vx.RegEx = Wt.RegEx (* and
                             Wt.RegEx = Pos.RegEx *)) : PARSER =
struct
  structure RegEx = Vx.RegEx
  (* structure Table = MkTreapTable(structure HashKey = Vx) *)
  open RegEx
  infix 5 cons
  infix 5 or

  open S (* RegEx.S *)

  type out = ((Vx.t * Vx.t * Wt.t) Seq.seq)

  exception ParseErr

  val space = plus (class Char.isSpace)
  val spaceq = star (class Char.isSpace)

  fun conss (x,y) = x cons spaceq cons y
  fun stars r = star (spaceq cons r)
  fun pluss r = plus (spaceq cons r)
  infix 5 conss

  val id = star (class (not o Char.isSpace))
  val node_id = Vx.regex

  val node_sep = str "--" or str "->"
  val stmt_sep = chr #";" or chr #" "
  val attr_sep = chr #","

  val graph_type = str "graph " or str "digraph "

  val weightattr = 
  chr #"[" conss
    maybe (str "weight" conss chr #"=" conss Wt.regex) conss
  chr #"]"
(*
  val posattr = 
  chr #"[" conss
    maybe (str "pos" conss chr #"=" conss Pos.regex) conss
  chr #"]"
*)
    
  val node_stmt = node_id (* conss maybe posattr *)
  val edge_stmt =
    node_id cons pluss (node_sep conss node_id) conss maybe weightattr

  val stmt = node_stmt or edge_stmt
  val stmt_list =
    stmt cons stars (stmt_sep conss stmt) cons maybe (spaceq cons stmt_sep)

  val graph = spaceq cons
    graph_type conss id conss
    chr #"{" conss
      stmt_list conss
    chr #"}" cons spaceq

  fun parseWeight s =
    let
      val weightattr = 
      chr #"[" conss
        maybe (str "weight" conss chr #"=" conss track Wt.regex) conss
      chr #"]"

      val st = retrieveAll (valOf (match weightattr s))
                handle Option => raise ParseErr
    in
      case length st of
        0 => Wt.default
      | _ => Wt.fromString (nth st 0)
    end

(*
  fun parseNode s = raise DoNotUse
    let
      val node_id = track Vx.regex
      val posattr = 
      chr #"[" conss
        maybe (str "pos" conss chr #"=" conss track Pos.regex) conss
      chr #"]"
      val node_stmt = node_id conss maybe posattr

      val store = retrieveAll (valOf (match node_stmt s))
                  handle Option => raise ParseErr
    in
      case length store of
        1 => Table.empty ()
      | 2 => Table.singleton (Vx.fromString (nth store 0),
                              Pos.fromString (nth store 1))
      | _ => raise ParseErr
    end
    handle ParseErr => Table.empty ()
*)
    
  fun parseEdges undirected s =
    let
      val node_id = track Vx.regex
      val edge_stmt' = node_id cons pluss (node_sep conss node_id)
      val edge_stmt = edge_stmt' conss track weightattr

      val matchd = match edge_stmt s

      val wt =
        case matchd of
          NONE => Wt.default
        | SOME st =>
            let val st' = retrieveAll st
            in  parseWeight (nth st' (length st' - 1))
            end

      val store =
        case matchd of
          SOME st =>
            let val st' = retrieveAll st
            in  take (st', length st' - 1)
            end
        | NONE => retrieveAll (valOf (match edge_stmt' s))
        handle Option => raise ParseErr

      val paired =
        tabulate (fn i => (Vx.fromString (nth store i),
                           Vx.fromString (nth store (i+1)),
                           wt))
                 (length store - 1)
    in
      if   undirected
      then append (paired, map (fn (x,y,w) => (y,x,w)) paired)
      else paired
    end
    handle ParseErr => empty ()
      
  fun parse s =
    let
      val graph_type = track (str "graph " or str "digraph ")

      val stmt = node_stmt or track edge_stmt
      val stmt_list =
        stmt cons stars (stmt_sep conss stmt) cons maybe (spaceq cons stmt_sep)

      val graph = spaceq cons
        graph_type conss id conss
        chr #"{" conss
          maybe stmt_list conss
        chr #"}" cons spaceq

      val store = retrieveAll (valOf (match graph s))
                  handle Option => raise ParseErr

      val undirected =
        case String.compare (nth store 0, "graph ") of
          EQUAL => true
        | _ => false

      val edges = flatten (map (parseEdges undirected) (drop (store, 1)))
    (*
      val nodes = reduce (Table.merge (fn (x,y) => y)) (Table.empty ())
                  (map parseNode (drop (store, 1)))
      fun dist x y =
        let
          val xPos = valOf (Table.find nodes x) handle Option => Pos.default
          val yPos = valOf (Table.find nodes y) handle Option => Pos.default
        in
          d (xPos, yPos)
        end
    *)
    in
     ((Seq.fromList o toList) edges)
    end

    fun parseFile fname = parse (TextIO.inputAll (TextIO.openIn fname))
end
