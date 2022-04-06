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

type variable2 = Variable of ctype * string (* a variable is a type and a name *) 

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


type expr3 = Sequence of expr3 * expr3 (* effectively a list *)
           | Brackets3 of expr3 (* for { } brackets *)
           | RoundBrackets of expr3 (* for ( ) brackets *)
           | Function of ctype * string * variable2 list* expr3 (*a function has a type a name , a lis tof arguments and a body *)
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
                Sequence (Brackets3 (fst parsed),
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
    | Brackets3 x -> Brackets3 (processUnprocessed (debth + 1) x)
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
    |  Brackets3 x -> replicate "   " tabs ^"{\n" 
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
    |  Brackets3 x ->  replicate "   " tabs^"Brackets:" ^"{\n"^ replicate "   " tabs
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



let _expr2ToString exp =
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

let _parser = 
  fix (fun expr ->
      let brackets2 = lift (fun x -> Brackets2 x) (char '('*> expr <* char ')')  in
      let plus2 = brackets2 <|> char '+' *> return (Plus "++") in
      let sequence2 = lift (fun x -> Sequence x) ( many plus2 ) in
      sequence2)

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false



let whitespace = take_while is_whitespace

let is_letter c = Char.lowercase_ascii c
                  |> Char.code
                  |> fun c -> not (c < Char.code 'a'|| c > Char.code 'z')

let word = take_while1 (is_letter)

let p_comma_separated_list parser = lift2 (fun x y -> List.append x [y]) (many (parser <* whitespace <* char ',' <* whitespace)) ( parser)

let comma_separated_list_to_string tostr l = if (l == []) 
  then "" 
  else l 
       |> List.map tostr 
       |> (fun x -> intersperse x ",")
       |> String.concat ""

type value = Star
           | Num of int
           | Var of string

let integer =
  lift int_of_string (take_while1 (function '0' .. '9' -> true | _ -> false))

let p_value = 
  let star = char '*' *> return Star in 
  let int = lift (fun x -> Num x) integer in
  let var = lift (fun x -> Var x) word  in
  int <|>  star <|> var

let value_to_string v = match v with 
    Star -> "*"
  | Num x -> Int.to_string x
  | Var s -> s

let values_list_to_string l = if List.length l != 0 then "["^comma_separated_list_to_string value_to_string l ^"]" else ""

type datatype = Int of value list

let datatype_to_string d = match d with
    Int list -> "int" ^ values_list_to_string list

let p_value_list = p_comma_separated_list p_value 

let p_datatype = lift (fun x -> Int x) (string "int" *> char '[' *> p_value_list <* char ']')<|>
                 string "int" *> return (Int [])


type expr = Plus of expr * expr
          | Value of value
          | Brackets of expr
          | Function_call of string * expr


let expr_to_string expr = 
  let rec tostr expr = match expr with 
    | Plus (e1,e2)-> "("^tostr e1 ^" + "^ tostr e2^")"
    | Value v -> value_to_string v
    | Brackets e -> "("^tostr e^")"
    | Function_call (name, expr) -> name ^"("^ tostr expr ^ ")"
  in 
  tostr expr


(*TODO: try to see if the order of secuential + can go from left to right. (so (a+b)+c instead of a + (b + c) ) note: can probably only be done by parsing backwards. maybe fix this afer processing???*)
(*TODO: add boolean logic operators *)
(*TODO: add with loops *)
let p_expr = 
  fix (fun expr ->
      let value = lift (fun x -> Value x) p_value in
      let brackets = lift (fun x -> Brackets x) (char '('*> whitespace *> expr <* whitespace <* char ')') in
      let function_call = lift2 (fun x y -> Function_call (x,y)) word (char '(' *> whitespace *> expr <* whitespace <* char ')')in
      let plus = lift2 (fun x y -> Plus (x,y)) ((function_call <|> brackets <|> value) <* whitespace <* char '+') (whitespace *> expr) in (* (brackets <|> value) is expr but without the plus to avoid infinite loops *)
      plus <|> function_call <|> brackets  <|> value
    )

type variable = Variable of datatype * string

let variable_to_string v = match v with 
    Variable (datatype, name) -> datatype_to_string datatype ^" "^name

let p_variable = lift2 (fun x y -> Variable (x,y)) p_datatype (whitespace *> word <* whitespace)

type program = Import of string * string
             | Var of variable
             | Return of expr
             | Function of datatype * string * variable list * program 
             | Assignment of string * expr
             | If of expr * program * program option
             | Sequence of program list

let program_to_string p = 
  let rec program_to_string' p tabs = 
    let tabs_string = replicate "    " tabs in
    match p with 
      Import (s1, s2) -> tabs_string ^ "use " ^s1 ^ " : "^s2^";"
    | Var variable -> tabs_string ^ variable_to_string variable^";"
    | Return e -> tabs_string ^ "return " ^expr_to_string e ^ ";"
    | Function (datatype, name, inputs, program) -> tabs_string ^ datatype_to_string datatype ^ " "^ name ^ "(" ^ comma_separated_list_to_string variable_to_string inputs ^")\n"
                                                    ^tabs_string ^ "{\n" 
                                                    ^tabs_string ^  program_to_string' program (tabs+1)^ "\n" 
                                                    ^tabs_string ^ "}\n"
    | Assignment (word, expr) -> tabs_string ^ word ^" = " ^expr_to_string expr ^ ";"
    | Sequence l -> List.map (fun x -> program_to_string' x tabs) l |> fun x -> intersperse x "\n" |> String.concat ""
    | If (cond , block1, opt_block2) -> tabs_string ^ "if("^ expr_to_string cond ^")\n"
                                        ^ tabs_string ^ "{\n"
                                        ^ program_to_string' block1 (tabs+1) ^ "\n"
                                        ^ tabs_string ^ "}\n" ^
                                        (match opt_block2 with 
                                           Some block2 -> tabs_string ^ "else\n"
                                                          ^ tabs_string ^ "{\n"
                                                          ^ program_to_string' block2 (tabs+1) ^ "\n"
                                                          ^ tabs_string ^ "}\n"
                                         | None -> "")
  in

  program_to_string' p 0

let p_program = fix (fun program ->
    let p_return = lift (fun x -> Return x) (string "return" *> whitespace *> p_expr <* whitespace <* char ';') in
    let p_var = lift (fun x -> Var x) p_variable <* char ';' in
    let p_assignment = lift2 (fun x y -> Assignment (x,y)) (word <* whitespace <* char '=') (whitespace *> p_expr <* whitespace <* char ';') in 
    let p_import = lift2 (fun x y -> Import (x,y)) (string "use" *> whitespace *> word <* whitespace <* char ':' <* whitespace)  (word <* whitespace <* char ';')in
    let p_function = lift4 (fun a b c d -> Function (a,b,c,d)) p_datatype (whitespace *> word) (char '(' *> p_comma_separated_list p_variable <* char ')') (whitespace *> char '{' *> whitespace *> program <* whitespace <* char '}') in
    let p_if = (let p_only_if =  lift2 (fun x y -> If (x,y,None)) (string "if" *> whitespace *> char '('*> p_expr <* char ')'<* whitespace) 
                    (char '{' *> whitespace *> program <* whitespace <* char '}'<* whitespace)  in
                let p_if_else = lift3 (fun x y z -> If (x,y,Some z)) (string "if" *> whitespace *> char '('*> p_expr <* char ')'<* whitespace) 
                    (char '{' *> whitespace *> program <* whitespace <* char '}' <* whitespace) 
                    (string "else" *> whitespace *> char '{' *> whitespace *> program <* whitespace <* char '}'<* whitespace) in
                p_if_else <|> p_only_if)in
   let parse = p_if <|> p_assignment <|> p_return <|> p_function <|> p_import <|> p_var in
   lift (fun x -> Sequence x) (many1 (parse <* whitespace))
  )


let convert parser str  =
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

"int func(int x)
{
   return x(3) + 3;
}
int func(int x)
{
   if (dim(arr) + 0)
   {
      res = func(arr);
   }
   else
   {
      res = func(arr);
   }
}"
|> convert p_program
|> program_to_string
|>print_endline;;
