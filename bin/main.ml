open Angstrom;;


print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n"
(*add <| opposite of |> (reverse applicator) *)
let (<|) f a = f a;;

(*add terminal input *)
let usage_msg = "append -o <output> <inputfile>";;

let input_files = ref [] 

let output_file = ref ""

let anon_fun filename =
  input_files := filename :: !input_files

let speclist =
  [("-o", Arg.Set_string output_file, "Set output file name")]

let first l = match l with
    x :: _ -> x
  | [] -> "no files"


let inpath = ref "";; (*"/home/sijmen/OCaml/sacProcessing/sacFiles/input.sac"; *)
let outpath = ref "";; (*"/home/sijmen/OCaml/sacProcessing/sacFiles/output.sac"; *)

let () =
  Arg.parse speclist anon_fun usage_msg;
  (*do main *)
  inpath := (first (!input_files));
  outpath := !output_file;
  (* the next two line set the default patvh *)
  inpath := "/home/sijmen/OCaml/sacProcessing/sacFiles/input.sac"; 
  outpath := "/home/sijmen/OCaml/sacProcessing/sacFiles/output.sac";;

(*/add terminal input *)


(* file IO*)
let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s;;

let input = read_whole_file !inpath;;

let write_whole_file str = 
  let oc = open_out !outpath in 
  Printf.fprintf oc "%s" str;
  close_out;;
(* file IO*)


let rec intersperse l item = match l with 
    x1::x2::xs -> x1 :: item :: intersperse  (x2 :: xs) item
  | x1 :: [] -> [x1]
  | [] -> []


let firstString l = match l with 
    x::_ -> x
  |_ -> ""

let secondString l = match l with 
    _::x2::_ -> x2
  |_ -> ""

let stringToCharList s =
  let rec exp i l =
    if i < 0 
    then 
      l 
    else
      exp (i - 1) (s.[i] :: l) 
  in
  exp (String.length s - 1) [];;

let charListToString list =
  String.concat "" (List.map (String.make 1) list )

(* parse to expr *)
let _endsInSemicolon str = if (String.get str ((String.length str)-1) ) == ';' then true (* check if the string ends in ) *)
  else false

type ctype = Type of string

let ctypeToStr c = match c with 
    Type s -> String.trim s

type variable = Variable of ctype * string (* a variable is a type and a name *) 

let variabletostring var = match var with 
    Variable (t,s) -> ctypeToStr t ^ " " ^ s 

let variableListToString l = "(" ^ (String.concat "" (intersperse (List.map variabletostring l) ",")) ^ ")"

let makevariable l = Variable (Type (firstString l), (secondString l))

let _parsevariables str = let
  is_not_empty l = (List.length l) > 0 
  in 
  let 
    list = List.filter is_not_empty (List.map  (String.split_on_char ' ') (String.split_on_char ',' str))
  in 
  List.map makevariable list


type expr = Sequence of expr * expr (* effectively a list *)
          | Brackets of expr (* for { } brackets *)
          | RoundBrackets of expr (* for ( ) brackets *)
          | Function of ctype * string * variable list* expr (*a function has a type a name , a lis tof arguments and a body *)
          | Unprocessed of string (* for rext we have not or can not pocess yet *)
          | Import of string (* for imports at teh top of the file *)
          | Whitespace (* for whitespace *)

let parseExpr str = 
  let rec parseBrackets str carry prev = (* parse the brackets {} first,
                                            this function returns the expression it has parsed and what it still has to parse*)
    match str with 
      '{'::xs -> 
      let parsed = parseBrackets xs [] prev (* this parse will return onc eit finds a closing bracket. the text after the bracket is snd of the tuple it returns*)
      in 
      Sequence (Unprocessed (charListToString <| List.rev carry), (* the text *)
                Sequence (Brackets (fst parsed),
                          fst <| parseBrackets (snd parsed) [] '{')), [] (* here we handle the "what the expression still has to parse" so we can pass nothing ([])*)
    | '}' :: xs -> (* TODO: check if there are valid non whitespace *)
      if (prev == '{') then Unprocessed (charListToString <| List.rev carry), xs (* we found a closing bracket so we return here with the rest of the text as the second of the tuple *)
      else parseBrackets xs ('}' :: carry) prev
    | '('::xs -> 
      let parsed = parseBrackets xs [] prev (* this parse will return onc eit finds a closing round bracket. the text after the bracket is snd of the tuple it returns*)
      in 
      Sequence (Unprocessed (charListToString <| List.rev carry), (* the text *)
                Sequence (RoundBrackets (fst parsed),
                          fst <| parseBrackets (snd parsed) [] ')')), [] (* here we handle the "what the expression still has to parse" so we can pass nothing ([])*)
    | ')' :: xs -> if (prev == '(') then Unprocessed (charListToString <| List.rev carry), xs (* we found a closing round bracket so we return here with the rest of the text as the second of the tuple *)
      else parseBrackets xs (')' :: carry) prev
    | ';' :: xs ->
      let parsed = parseBrackets xs [] prev
      in
      Sequence (Unprocessed (charListToString <| List.rev (';'::carry)), (* we encountered a ';' so we add the carry as a sequence 
                                                                            (we also add the ';' back on to distinguish between function definitons and expressions later) *)
                fst parsed), snd parsed
    | x :: xs -> (* if we do not find a "special" character just add it to the carry *)
      parseBrackets xs (x :: carry) prev
    | _ -> 
      Unprocessed (charListToString <| List.rev carry),[]
  in
  let isFunction expr = match expr with 
      _ -> false

  in
  let rec processUnprocessed debth expr = (* process the unprocessed imput which should mostly be single lines ending in ';'*)
    match expr with 
      Unprocessed  s -> (match (stringToCharList <| String.trim s) with 
        'u' :: 's':: 'e':: _ -> Import s
        | [] -> Whitespace
        | _ -> Unprocessed s)
    | Sequence (x ,y) -> if isFunction (Sequence (x ,y)) (* if it is a function *)
      then 
        Sequence (processUnprocessed debth x, processUnprocessed debth y)
      else 
        Sequence (processUnprocessed debth x, processUnprocessed debth y)
    | Brackets x -> Brackets (processUnprocessed (debth + 1) x)
    | RoundBrackets x -> RoundBrackets x
    | Import x -> Import x
    | Whitespace -> Whitespace
    | Function (typ,name,list,expr) -> Function (typ,name,list,expr)
  in
  parseBrackets (stringToCharList str) [] ' ' 
  |> fst
  |> processUnprocessed 0;;

(* print expr *)

let replicate str i =
  let rec replicate' str i = if i > 0 
    then
      str ^ replicate' str (i-1)
    else
      ""
  in 
  replicate' str i;;

let exprToString expr =
  let rec exprToString' expr tabs = match expr with
      Sequence (x, y) -> exprToString' x tabs ^ exprToString' y tabs
    |  Brackets x -> replicate "   " tabs ^"{\n" 
                     ^ exprToString' x (tabs+1) 
                     ^ replicate "   " tabs^ "}"
    | RoundBrackets x -> "(" 
                         ^ exprToString' x tabs 
                         ^ ")"
    | Unprocessed x -> "/*unprocessed:"^ String.trim x ^"*/"^ String.trim x 
    | Import x -> "/*import:*/\n" ^ replicate "   " tabs ^ String.trim x 
    | Whitespace -> "/*Whitespace*/\n"
    | Function (typ,name,list,expr) -> "\n/*function " ^ name ^ "*/\n" ^ ctypeToStr typ ^ " " ^ name ^ variableListToString list ^ "\n" ^exprToString' expr tabs
  in
  exprToString' expr 0

let showExpr expr =
  let rec showExpr' expr tabs = match expr with
      Sequence (x, y) -> showExpr' x tabs ^"\n"^ showExpr' y tabs
    |  Brackets x ->  replicate "   " tabs^"Brackets:" ^"{\n"^ replicate "   " tabs
                      ^ showExpr' x (tabs+1) 
                      ^ replicate "   " tabs^ "}"
    | RoundBrackets x -> replicate "   " tabs^"(" 
                         ^ showExpr' x tabs 
                         ^ ")"
    | Unprocessed x -> "unprocessed:" ^ String.trim x 
    | Import _ -> "import:"
    | Whitespace -> "Whitespace"
    | Function (_,name,list,expr) -> "function " ^ name ^ variableListToString list ^ "\n"^ replicate "   " tabs ^showExpr' expr tabs
  in
  "\n\n/*"^( showExpr' expr 0) ^"*/"

let processinput str = parseExpr str;;




type expr2 =  Brackets2 of expr2 
           | Plus of string
           | Sequence of expr2 list

let _fuckerrors = Brackets2 (Plus "d")

let expr2ToString exp =
  let rec expr2ToString' exp = match exp with
      Brackets2 exp -> "(" ^ (expr2ToString' exp) ^ ")"
    | Sequence l -> List.map expr2ToString' l 
                    |> (fun x -> intersperse x " then ") 
                    |> List.fold_left (^) ""
    | Plus s -> s
  in
  expr2ToString' exp

let cons x xs = x :: xs

let _parse_seq first rest = 
  lift2 (cons) first (many rest)

let parser = 
  fix (fun expr ->
      let brackets = char '('*> expr <* char ')'>>| (fun x -> Brackets2 x) in
      let plus = brackets <|> char '+' *> return (Plus "++") in
      let sequence = ( many plus >>| (fun x -> Sequence x)) in
      sequence)



let convert str =
  match parse_string ~consume:All parser str with
  | Ok v      -> v
  | Error msg -> failwith msg
;;
processinput input 
|> exprToString
|> print_endline;;

processinput input 
|> (fun x -> exprToString x ^ showExpr x )
|> write_whole_file;;

print_endline "\n\n\n";

"((+)+)(+)"
|> convert 
|> expr2ToString
|>print_endline;;
