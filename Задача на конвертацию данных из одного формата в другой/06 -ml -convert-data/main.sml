(*
  CSV -> XML with schema + stats
  SML/NJ compatible
  Header format: colName:type, colName:type, ...
  Types: int | real | string
*)

(* =========================
   Types
   ========================= *)

datatype ColType = TInt | TReal | TString

type Column = { name : string, ctype : ColType }

type Stats = { min : real option, max : real option }

(* =========================
   Small helpers
   ========================= *)

fun split (s: string, delim: char) : string list =
  let
    fun loop ([], cur, acc) = rev (String.implode (rev cur) :: acc)
      | loop (c::cs, cur, acc) =
          if c = delim
          then loop (cs, [], String.implode (rev cur) :: acc)
          else loop (cs, c::cur, acc)
  in
    (* Special-case empty string to return [""] like many split functions *)
    if s = "" then [""]
    else loop (String.explode s, [], [])
  end

(* remove trailing \n and optional \r (Windows) *)
fun trimLine (s: string) : string =
  let
    fun drop1 x =
      if size x > 0 andalso String.sub (x, size x - 1) = #"\n"
      then String.substring (x, 0, size x - 1)
      else x
    fun drop2 x =
      if size x > 0 andalso String.sub (x, size x - 1) = #"\r"
      then String.substring (x, 0, size x - 1)
      else x
  in
    drop2 (drop1 s)
  end

fun fail msg = raise Fail msg

(* XML escaping *)
fun escapeXML (s: string) : string =
  String.translate
    (fn #"&"  => "&amp;"
      | #"<"  => "&lt;"
      | #">"  => "&gt;"
      | #"\"" => "&quot;"
      | #"'"  => "&apos;"
      | c     => str c) s

(* =========================
   Schema parsing
   ========================= *)

fun parseType (t: string) : ColType =
  case t of
      "int"    => TInt
    | "real"   => TReal
    | "string" => TString
    | _        => fail ("Unknown column type: " ^ t)

fun parseColumn (s: string) : Column =
  case split (s, #":") of
      [name, t] => { name = name, ctype = parseType t }
    | _ => fail ("Invalid schema token: " ^ s ^ " (expected name:type)")

fun parseSchema (headerTokens: string list) : Column list =
  map parseColumn headerTokens

fun typeToString TInt = "int"
  | typeToString TReal = "real"
  | typeToString TString = "string"

(* =========================
   Validation + stats
   ========================= *)

fun parseIntOrFail (colName: string, value: string) : int =
  case Int.fromString value of
      SOME v => v
    | NONE => fail ("Invalid int in column '" ^ colName ^ "': " ^ value)

fun parseRealOrFail (colName: string, value: string) : real =
  case Real.fromString value of
      SOME v => v
    | NONE => fail ("Invalid real in column '" ^ colName ^ "': " ^ value)

fun updateStats (col: Column, value: string, st: Stats) : Stats =
  case #ctype col of
      TInt =>
        let
          val r = Real.fromInt (parseIntOrFail (#name col, value))
          val newMin =
            (case #min st of NONE => SOME r | SOME m => SOME (Real.min (m, r)))
          val newMax =
            (case #max st of NONE => SOME r | SOME m => SOME (Real.max (m, r)))
        in
          { min = newMin, max = newMax }
        end
    | TReal =>
        let
          val r = parseRealOrFail (#name col, value)
          val newMin =
            (case #min st of NONE => SOME r | SOME m => SOME (Real.min (m, r)))
          val newMax =
            (case #max st of NONE => SOME r | SOME m => SOME (Real.max (m, r)))
        in
          { min = newMin, max = newMax }
        end
    | TString =>
        (* no numeric stats *)
        st

fun initStats (schema: Column list) : Stats list =
  map (fn _ => { min = NONE, max = NONE }) schema

(* fold stats across columns: schema + rowValues + currentStats *)
fun updateStatsForRow (schema: Column list, values: string list, stats: Stats list) : Stats list =
  let
    (* validate length early *)
    val _ =
      if length values <> length schema
      then fail "Column count mismatch in a row"
      else ()
  in
    (* fold left over (schema, value, stat) triples *)
    ListPair.map
      (fn (col, (value, st)) => updateStats (col, value, st))
      (schema, ListPair.zip (values, stats))
  end

(* =========================
   Read CSV
   ========================= *)

fun readCSV (path: string) : (Column list * (string list) list * Stats list) =
  let
    val ins = TextIO.openIn path

    val headerLine =
      (case TextIO.inputLine ins of
          NONE => (TextIO.closeIn ins; fail "Empty CSV file")
        | SOME l => trimLine l)

    val schema = parseSchema (split (headerLine, #","))

    val stats0 = initStats schema

    fun loop (rowsAcc: (string list) list, statsAcc: Stats list) =
      case TextIO.inputLine ins of
          NONE => (rev rowsAcc, statsAcc)
        | SOME l =>
            let
              val line = trimLine l
              (* allow skipping completely empty lines *)
              val _ = ()
              val values = split (line, #",")
              val stats' = updateStatsForRow (schema, values, statsAcc)
            in
              loop (values :: rowsAcc, stats')
            end

    val (rows, finalStats) = loop ([], stats0)
    val _ = TextIO.closeIn ins
  in
    (schema, rows, finalStats)
  end

(* =========================
   XML generation
   ========================= *)

fun xmlField (col: Column, value: string) : string =
  "    <" ^ #name col ^
  " type=\"" ^ typeToString (#ctype col) ^ "\">" ^
  escapeXML value ^
  "</" ^ #name col ^ ">\n"

fun xmlRow (schema: Column list, row: string list) : string =
  "  <row>\n" ^
  String.concat (ListPair.map xmlField (schema, row)) ^
  "  </row>\n"

fun xmlStatsOne (col: Column, st: Stats) : string =
  case (#ctype col, #min st, #max st) of
      (TInt, SOME mn, SOME mx) =>
        "    <column name=\"" ^ #name col ^
        "\" type=\"int\" min=\"" ^ Real.toString mn ^
        "\" max=\"" ^ Real.toString mx ^ "\" />\n"
    | (TReal, SOME mn, SOME mx) =>
        "    <column name=\"" ^ #name col ^
        "\" type=\"real\" min=\"" ^ Real.toString mn ^
        "\" max=\"" ^ Real.toString mx ^ "\" />\n"
    | _ => ""  (* no stats for string or empty numeric columns *)

fun csvToXML (schema: Column list, rows: (string list) list, stats: Stats list) : string =
  "<rows count=\"" ^ Int.toString (length rows) ^ "\">\n" ^
  "  <meta>\n" ^
  String.concat (ListPair.map xmlStatsOne (schema, stats)) ^
  "  </meta>\n" ^
  String.concat (map (fn r => xmlRow (schema, r)) rows) ^
  "</rows>\n"

(* =========================
   Write file
   ========================= *)

fun writeText (path: string, content: string) : unit =
  let
    val out = TextIO.openOut path
    val _ = TextIO.output (out, content)
    val _ = TextIO.closeOut out
  in
    ()
  end

(* =========================
   Public API
   ========================= *)

fun convert (csvFile: string, xmlFile: string) : unit =
  let
    val (schema, rows, stats) = readCSV csvFile
    val xml = csvToXML (schema, rows, stats)
    val _ = writeText (xmlFile, xml)
  in
    print ("✓ Done! Created " ^ xmlFile ^ "\n")
  end

(*
  Usage:
    use "main.sml";
    convert ("input.csv", "output.xml");
*)
