structure Tester : TESTER =
struct
  exception FailInputOutputFileFormat

  val defaultTime = ref (Time.fromSeconds 2)
  val timeout = ref (!defaultTime)

  fun setTimeout t = timeout := (Time.fromSeconds t)
  fun setDefaultTimeout t = defaultTime := (Time.fromSeconds t)
  fun restore () = timeout := !defaultTime

  fun dropNewLine s =
      case String.sub (s, (String.size s)-1) of
          #"\n" => String.substring (s, 0, (String.size s)-1)
        | _ => s

  fun toList ins fromString =
      case TextIO.inputLine ins  of
          NONE => (TextIO.closeIn ins; nil)
        | SOME s =>
          case String.explode s of
              #"/"::(#"/"::_) => toList ins fromString
            | _ => case (fromString (dropNewLine s)) of
                       NONE => (toList ins fromString)
                     | SOME s => s::(toList ins fromString)

  fun testCase h exp inToString outToString cmp f =
      let val act = f h
      in if cmp (act,exp) then
           (NONE, "Test Passed")
         else
           (SOME exp, "Test Failed on input: " ^ (inToString h) ^ " got: " ^
                      (outToString act) ^ " but wanted: " ^ (outToString exp))
      end

  fun testFromRef (inToString : 'a -> string)
                  (outToString : 'b -> string)
                  cmp f fRef inputs =
      let fun one h = testCase h (fRef h) inToString outToString cmp f
      in List.map one inputs
      end


  fun testFromRefFile (inToString, inFromString) outToString
                      cmp f fRef inputFileName =
      let val ins = TextIO.openIn inputFileName
          val inputs = toList ins inFromString
      in testFromRef inToString outToString cmp f fRef inputs
      end


  fun testFromOutput inToString outToString cmp f inputs =
      let fun one (i, out) = testCase i out inToString outToString cmp f
      in List.map one inputs
      end

  fun testFromOutputFile (inToString, inFromString)
                         (outToString, outFromString)
                         cmp f (inputFileName,outputFileName)  =
      let val ins = TextIO.openIn inputFileName
          val outs = TextIO.openIn outputFileName
          val inputs = ListPair.zipEq (toList ins inFromString,
                                       toList outs outFromString)
      in testFromOutput inToString outToString cmp f inputs
      end
end
