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

let tuple_first t = match t with
    (a,_) -> a

let tuple_second t = match t with
    (_,b) -> b

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
          | ExprWithIndex of expr * expr list (* things of type arr[1,3] *)
          | Dot
          | MDot

let expr_to_string expr tabs = 
  let rec tostr expr = match expr with 
    | Plus (e1,e2)-> "("^tostr e1 ^" + "^ tostr e2^")"
    | Value v -> value_to_string v
    | Brackets e -> "("^tostr e^")"
    | Function_call (name, exprList) -> name ^"("^ String.concat "," (List.map tostr exprList) ^ ")"
    | Boolean (e1,operator,e2) -> "("^tostr e1 ^" "^ operator ^ " "^ tostr e2^")"
    | With_loop (typeset,gen_exprs,operations) -> "with\n"^tabs^"{\n" ^ tabs^"   "^ tostr typeset ^ " : " ^ tostr gen_exprs ^ ";\n"^tabs ^ "}:" ^ tostr operations 
    | Array exprList -> "[" ^ String.concat "," (List.map tostr exprList) ^ "]"
    | Dot -> "."
    | MDot -> "..."
    | ExprWithIndex (expr,exprList )-> tostr expr ^ "[" ^ String.concat "," (List.map tostr exprList) ^ "]"
  in 
  tostr expr

let rec unpack_brackets expr = match expr with 
    Brackets e  -> unpack_brackets e
  | _ -> expr 

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

type dependency = 
    Dependencies of string list * bool * int option (*function calls , is it a recusive function, is it a dimentionality conditional and if so for which dimentionality*)
  | No_Dependency
  | Not_Loaded
  | Error

let dependency_is_rec dep = match dep with
    Dependencies (_,x,_) -> x
  | _ -> false

let dependency_to_string dependency = match dependency with 
    Dependencies (list , recusivce, None ) -> (if recusivce then "recursive! " else "") ^ ("function calls: " ^ String.concat ", " list) 
  | Dependencies (list , recusivce, Some i ) -> (if recusivce then "recursive! " else "") ^ ("function calls: " ^ String.concat ", " list) ^ " | dimentionality conditional:" ^ Int.to_string i
  | No_Dependency -> "no dependency"
  | Not_Loaded -> "dependency not loaded."
  | Error -> "ERROR COMPUTING DEPENDENCIES!"

type program = Import of string * string * dependency
             | Var of variable * dependency
             | Return of expr * dependency
             | Function of datatype * string * variable list * program * dependency
             | Assignment of string * expr * dependency
             | If of expr * program * program option * dependency
             | Sequence of program list * dependency

let program_to_string p = 
  let rec program_to_string' p tabs = 
    let tabs_string = replicate "   " tabs in
    match p with 
      Import (s1, s2, dep) -> tabs_string ^ "use " ^s1 ^ " : "^s2^"; // " ^ dependency_to_string dep
    | Var (variable, dep) -> tabs_string ^ variable_to_string variable^"; // " ^ dependency_to_string dep
    | Return (e, dep) -> tabs_string ^ "return " ^expr_to_string e tabs_string^ "; // " ^ dependency_to_string dep
    | Function (datatype, name, inputs, program, dep) -> tabs_string ^ datatype_to_string datatype ^ " "^ name ^ "(" ^ comma_separated_list_to_string variable_to_string inputs ^") // "^ dependency_to_string dep^"\n"
                                                         ^tabs_string ^ "{\n" 
                                                         ^tabs_string ^  program_to_string' program (tabs+1)^ "\n" 
                                                         ^tabs_string ^ "}\n"
    | Assignment (word, expr, dep) -> tabs_string ^ word ^" = " ^expr_to_string expr tabs_string^ "; // " ^ dependency_to_string dep
    | Sequence (l, _) -> (List.map (fun x -> program_to_string' x tabs) l |> fun x -> intersperse x "\n" |> String.concat "" )^ "//seq"
    | If (cond , block1, opt_block2, dep) -> tabs_string ^ "if ("^ expr_to_string cond tabs_string ^") // "^ dependency_to_string dep ^"\n"
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
    let p_return = lift (fun x -> Return (x,Not_Loaded)) (string "return" *> whitespace *> p_expr <* whitespace <* char ';') in
    let p_var = lift (fun x -> Var (x,Not_Loaded)) p_variable <* char ';' in
    let p_assignment = lift2 (fun x y -> Assignment (x,y,Not_Loaded)) (word <* whitespace <* char '=') (whitespace *> p_expr <* whitespace <* char ';') in 
    let p_import = lift2 (fun x y -> Import (x,y,Not_Loaded)) (string "use" *> whitespace *> word <* whitespace <* char ':' <* whitespace)  (word <* whitespace <* char ';')in
    let p_function = lift4 (fun a b c d -> Function (a,b,c,d,Not_Loaded)) p_datatype (whitespace *> word) (char '(' *> p_comma_separated_list p_variable <* char ')') (whitespace *> char '{' *> whitespace *> program <* whitespace <* char '}') in
    let p_if = (let p_only_if =  lift2 (fun x y -> If (x,y,None,Not_Loaded)) (string "if" *> whitespace *> char '('*> p_expr <* char ')'<* whitespace) 
                    (char '{' *> whitespace *> program <* whitespace <* char '}'<* whitespace)  in
                let p_if_else = lift3 (fun x y z -> If (x,y,Some z,Not_Loaded)) (string "if" *> whitespace *> char '('*> p_expr <* char ')'<* whitespace) 
                    (char '{' *> whitespace *> program <* whitespace <* char '}' <* whitespace) 
                    (string "else" *> whitespace *> char '{' *> whitespace *> program <* whitespace <* char '}'<* whitespace) in
                p_if_else <|> p_only_if)in
    let parse = p_if <|> p_assignment <|> p_return <|> p_function <|> p_import <|> p_var in
    lift (fun x -> Sequence (x,Not_Loaded)) (many1 (parse <* whitespace))
  )

let list_append_unique l1 l2 =
  let exist l1 s = List.exists (fun x -> String.equal x s) l1 in
  let rec apend l1 l2 = match l2 with 
      x :: xs -> if exist l1 x then apend l1 xs else apend (x::l1) xs
    | [] -> l1
  in apend l1 l2

let combine_dependencies dep1 dep2 = match (dep1,dep2) with 
    (Dependencies (l1,_ ,_), Dependencies (l2,_ ,_)) -> Dependencies ((list_append_unique l1 l2), false, None)
  | (Dependencies (l1,_ ,_) , _) -> Dependencies (l1, false, None)
  | (_, Dependencies (l2,_,_)) -> Dependencies (l2, false, None)
  | (No_Dependency,_) -> No_Dependency
  | (_,No_Dependency) -> No_Dependency
  | _ -> Error

let expr_to_dependency expr =
  let nothing = No_Dependency in

  let rec expr_to_dependency' expr = 
    (let list_to_dep l = List.fold_left combine_dependencies nothing (List.map expr_to_dependency' l) in 
     match expr with 
     | Plus (e1,e2)-> combine_dependencies (expr_to_dependency' e1) (expr_to_dependency' e2)
     | Value _ -> nothing
     | Brackets e -> (expr_to_dependency' e)
     | Function_call (name, exprList) -> combine_dependencies (Dependencies ([name],false,None)) (list_to_dep exprList)
     | Boolean (e1,_,e2) -> combine_dependencies (expr_to_dependency' e1) (expr_to_dependency' e2)
     | With_loop (typeset,gen_exprs,operations) -> combine_dependencies (combine_dependencies (expr_to_dependency' typeset) (expr_to_dependency' gen_exprs)) (expr_to_dependency' operations)
     | Array exprList -> (list_to_dep exprList)
     | Dot -> nothing
     | MDot -> nothing
     | ExprWithIndex (expr, exprList )-> combine_dependencies (expr_to_dependency' expr) (list_to_dep exprList) )
  in
  expr_to_dependency' expr

(* takes a name and a dependency and returns a dependency if the name is in the list of function calls *)
let is_recursive name dependency = match dependency with
    Dependencies (list , _, _) -> Dependencies (list , List.exists (String.equal name) list, None)
  | dep -> dep

(* takes a expression and a dependency and modifies the dependency based of in the expression is a dimentionality conditional. *)
let is_dimentionality_conditional expr dep =
  let either e1 e2 f = match f e1 e2 with
    | Some s -> Some s
    | None -> f e2 e1 in
  let is_condition str = List.exists (String.equal str) ["=="; "<="] in
  let if_dim_call expr = match unpack_brackets expr with (* check for dim(whatever) *)
      Function_call ("dim" ,_) -> true
    | _ -> false in
  let get_int expr = match unpack_brackets expr with (* check for some number *)
      Value (Num int) -> Some int
    | _ -> None in
  let is_dim_cond expr = match unpack_brackets expr with 
      Boolean (e1, s, e2)-> if is_condition s then either e1 e2 (fun e1 e2 -> if if_dim_call e1 then get_int e2 else None  )  else None
    | _ -> None in
  match is_dim_cond expr with 
    Some i -> (match dep with 
        Dependencies (list, boolean, _) -> Dependencies (list, boolean, (Some i))
      | _ -> Dependencies ([],false,Some i))
  | None -> dep

let loadDependencies program = 
  let rec loadDependencies' program = 
    let process_list l = let new_list = List.map loadDependencies' l in (List.map tuple_first new_list ,List.fold_left combine_dependencies Not_Loaded (List.map tuple_second new_list)) in
    match program with (* returns a tuple of a program with the dependencies sored in it and the last aggragate dependency *)
      Import (s1, s2, _) -> (Import (s1, s2, No_Dependency), No_Dependency)
    | Var (variable, _) -> (Var (variable, No_Dependency), No_Dependency)
    | Return (e, _) -> (let new_dep = expr_to_dependency e in
                        (Return (e, new_dep),new_dep))
    | Function (datatype, name, inputs, program, _) ->(let preprocessed = loadDependencies' program in 
                                                       let new_dep = tuple_second preprocessed
                                                       in (Function (datatype, name, inputs,tuple_first preprocessed, is_recursive name new_dep),new_dep))
    | Assignment (word, expr, _) -> (let new_dep = expr_to_dependency expr
                                     in (Assignment (word, expr, new_dep),new_dep))
    | Sequence (list, _) -> (let preprocessed_list = process_list list in
                             (Sequence (tuple_first preprocessed_list, tuple_second preprocessed_list), tuple_second preprocessed_list))
    | If (cond , block1, opt_block2, _) -> 
      (match opt_block2 with 
         Some block2 -> (let new_dep = is_dimentionality_conditional cond (combine_dependencies (combine_dependencies (tuple_second (loadDependencies' block1)) (tuple_second (loadDependencies' block2))) (expr_to_dependency cond)) in
                         (If (cond ,tuple_first (loadDependencies' block1), Some (tuple_first (loadDependencies' block2)), new_dep),new_dep))
       | None -> (let new_dep = (tuple_second (loadDependencies' block1)) in
                  (If (cond ,tuple_first (loadDependencies' block1), None, new_dep),new_dep)))
  in
  tuple_first (loadDependencies' program)

let eliminate_recursion program = 
  let is_dumb_index_set expr = match unpack_brackets expr with
      Boolean (Dot,"<=",Boolean(Array _ ,"<",Function_call("take",_))) -> true (*TODO: test for [1], shape(arr) *)
    | _ -> false in 
  let is_dumb_with_loop expr rec_fun_name = match expr with 
      With_loop (index_set, Function_call (name,_) , Function_call("modarray",_)) -> is_dumb_index_set index_set && String.equal name rec_fun_name (*TODO: check if modaray mods the same array *)
    | _ -> false in
  let expr_to_with_function_call expr = match expr with 
      Function_call (name,[Value (Var arr)]) -> Some ((Function_call (name, [ExprWithIndex (Value (Var arr), [Value (Var "iv")])] )),arr)
    | _ -> None in
  let generate_with_loop expr = match expr with
      Some (expr,arr) -> Some (With_loop (Boolean (Dot,"<=",Boolean(Value (Var "iv") ,"<",Function_call("shape",[Value (Var arr)]))), expr, Function_call ("modarray",[Value (Var arr)])))
    | None -> None in
  let replace_if_statement program return_value rec_fun_name = match program with 
      If (cond , Sequence ([Assignment (name, expr,dep)],dep2), Some Sequence ([Assignment (name2, expr2,dep3)],dep4), Dependencies (list,b,int)) -> 
      (let do_nothing = If (cond , Sequence ([Assignment (name, expr,dep)],dep2), Some (Sequence ([Assignment (name2, expr2,dep3)],dep4)), Dependencies (list,b,int)) 
       in  
       if (String.equal name name2 && String.equal name return_value && is_dumb_with_loop expr2 rec_fun_name)
       then (match generate_with_loop (expr_to_with_function_call expr) with 
             Some with_loop -> Assignment (name ,with_loop,Dependencies (list,b,int)) 
           | _  -> do_nothing)
       else do_nothing)
    | x -> x  in
  let get_return program = match program with 
      Return (Value (Var x), _ ) -> x
    | _ -> "" in
  let replace_function_sequence program rec_fun_name= match program with 
      Sequence (list, dep) -> (match list with 
          if_statement :: return_statement :: [] -> Sequence ([replace_if_statement if_statement (get_return return_statement) rec_fun_name ;return_statement], dep)
        | _ -> Sequence ([], dep))
    | x -> x 
  in
  let rec eliminate_recursion' program = match program with
      Sequence (list, dep) -> Sequence( List.map eliminate_recursion' list, dep)
    | Function (datatype, name, inputs, program, dep) -> if dependency_is_rec dep then 
        Function (datatype, name, inputs, replace_function_sequence program name, dep) 
      else Function (datatype, name, inputs, program, dep)
    | x -> x
  in
  eliminate_recursion' program


let convert parser str  =
  match parse_string ~consume:All parser str with
  | Ok v      -> v
  | Error msg -> failwith msg
;;

convert p_program input
|> loadDependencies
|> eliminate_recursion
|> program_to_string
|> write_whole_file;;