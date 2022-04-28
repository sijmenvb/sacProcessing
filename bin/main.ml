open Angstrom;;


print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n"

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



let replicate str i =
  let rec replicate' str i = if i > 0 
    then
      str ^ replicate' str (i-1)
    else
      ""
  in 
  replicate' str i;;


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
          | Boolean of expr * string * expr
          | Value of value
          | Brackets of expr
          | Function_call of string * expr list
          | With_loop of expr * expr * expr (*of type   with {expr : expr ;} : expr*)
          | Array of expr list
          | ExprWithIndex of expr * expr list
          | Dot
          | MDot

let expr_to_string expr = 
  let rec tostr expr = match expr with 
    | Plus (e1,e2)-> "("^tostr e1 ^" + "^ tostr e2^")"
    | Value v -> value_to_string v
    | Brackets e -> "("^tostr e^")"
    | Function_call (name, exprList) -> name ^"("^ String.concat "," (List.map tostr exprList) ^ ")"
    | Boolean (e1,operator,e2) -> "("^tostr e1 ^" "^ operator ^ " "^ tostr e2^")"
    | With_loop (typeset,gen_exprs,operations) -> "with {" ^ tostr typeset ^ " : " ^ tostr gen_exprs ^ "; " ^ tostr operations ^ ";"
    | Array exprList -> "[" ^ String.concat "," (List.map tostr exprList) ^ "]"
    | Dot -> "."
    | MDot -> "..."
    | ExprWithIndex (expr,exprList )-> tostr expr ^ "[" ^ String.concat "," (List.map tostr exprList) ^ "]"
  in 
  tostr expr


(*TODO: try to see if the order of secuential + can go from left to right. (so (a+b)+c instead of a + (b + c) ) note: can probably only be done by parsing backwards. maybe fix this afer processing???*)
(*TODO: add with loops *)
let p_expr = 
  let boolean_seperator = string "==" <|> string "<=" <|>string ">=" <|>string "<" <|> string ">" <|> string "&&" <|> string "==" in
  fix (fun expr ->
      let value = lift (fun x -> Value x) p_value 
      in
      let brackets = lift (fun x -> Brackets x) (char '('*> whitespace *> expr <* whitespace <* char ')')
      in
      let function_call = lift2 (fun x y -> Function_call (x,y)) word (char '(' *> p_comma_separated_list (whitespace *> expr) <* whitespace <* char ')')
      in
      let with_loop = lift3 (fun x y z-> With_loop (x,y,z)) (whitespace *> string "with" *> whitespace *> char '{' *> whitespace *> expr ) 
          (whitespace *> char ':' *> whitespace *> expr <* whitespace <* char ';') 
          ( whitespace *> char '}' *> whitespace *> char ':' *> expr <* whitespace)
      in
      let array = lift (fun x -> Array (x)) (char '[' *> p_comma_separated_list (whitespace *> expr) <* whitespace <* char ']')
      in
      let dot = lift (fun _ -> Dot) (char '.')
      in
      let mdot = lift (fun _ -> MDot) (string "...")
      in
      let single = mdot <|> dot <|> array <|> with_loop <|> function_call <|> brackets <|> value (* use of single to prevent infinite loop *)
      in
      let exprWithIndex = lift2 (fun x y -> ExprWithIndex (x,y)) (single) (p_comma_separated_list expr) 
      in
      let plus = lift2 (fun x y -> Plus (x,y)) ((exprWithIndex <|> single) <* whitespace <* char '+') (whitespace *> expr) 
      in 
      let boolean = lift3 (fun x y z-> Boolean (x,y,z)) (plus <* whitespace ) (boolean_seperator) (whitespace *> expr) 
                    <|> lift3 (fun x y z-> Boolean (x,y,z)) ((exprWithIndex <|> single) <* whitespace ) (boolean_seperator) (whitespace *> expr) (* i'm using it twice here since plus <|> single does not seem to work *)
      in 
      boolean <|> plus <|> exprWithIndex <|> single
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
    let tabs_string = replicate "   " tabs in
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
convert p_program input 
|> program_to_string
|> print_endline;;

convert p_program input 
|> program_to_string
|> write_whole_file;;