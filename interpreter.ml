(*Writing a line to a file*)

let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp 
let writeNothing (file_path: string): unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "" in
    close_out fp

(* commands*)
type const = | String of string | Int of int | Clo of string * string * string | Left of const | Right of const | Tuple of const list
type a= String of string
type b= Int of int

let rec const_toString (x : const) : string =
  let rec help (x: string list) : string =
    match x with 
    | [] -> ""
    | h::t-> if List.length t != 0 then h^", "^(help t) else h
     in

  match x with 
  | String(x) -> "\""^x^"\""
  | Int(x)-> string_of_int x
  | Clo(x, y, z)-> "Clo "^"("^x^", "^y^")"
  | Left(x)->"Left "^(const_toString x)
  | Right(x)->"Right "^(const_toString x)
  | Tuple(x)->"("^(help (List.map const_toString x))^")"


let print (stack: const list) (file_path: string) : unit=
  let fp = open_out file_path in
  let rec to_str (x : const list): unit =
    match x with
    | [] -> ()
    | h::t -> (match h with
               | String s -> Printf.fprintf fp "%s" ("\""^s^"\"")
               | Int i -> Printf.fprintf fp "%d" i
               | Clo (a, b, c) -> Printf.fprintf fp "%s" ("Clo ("^a^", "^b^")")
               | Left(a)-> Printf.fprintf fp "%s" ("Left "^(const_toString a))
               | Right (a)-> Printf.fprintf fp "%s" ("Right "^(const_toString a))
               | x-> Printf.fprintf fp "%s" (const_toString x)

               
               
               
               ); Printf.fprintf fp "%s\n" ""; to_str t
 in
  (to_str stack) ; close_out fp

let quit (stack: const list) (file_path: string): unit =
 print stack file_path

 let valid_string (c: string) : bool =
  let exp (c : char ) : bool =
    match ((int_of_char c) <=90 && (int_of_char c) >=65) || ((int_of_char c) <=122 && (int_of_char c) >=97) with
    | true -> true
    | false -> false
  in 
   String.fold_left (fun acc x -> exp x && acc) true c

 let push (stack : const list) (x : const) : const list =
  x::stack


(* parse string and return characters up to first space*)
let  reverse (x: 'a list)=
let rec help (x : 'a list) (acc: 'a list) =
  match x with 
  |[]-> acc 
  |h::t-> help t (h::acc) 
in help x []

let explode (s: string) : char list =
  reverse (String.fold_left (fun acc x -> x::acc) [] s) 
let implode (l: char list) : string =
  List.fold_left (fun acc x -> acc^(String.make 1 x)) "" l 
let command (x : string) : string =
let rec help2 (x : string) (acc: string): string =
  match explode x with
  | [] -> acc
  | h::t -> if h = ' ' || h='\n' then acc else help2 (implode t) (acc^(String.make 1 h)) in 
  help2 x ""
let file_argument (x : string) : string =
  let rec help2 (x : string) (acc: string): string =
    match explode x with
    | [] -> ""
    | h::t -> (match h with
    | ' ' -> implode (t)
    | '\n' ->(implode (h::t))
    | '\"'->  (implode (h::t))
    |_ -> help2 (implode t) (acc^(String.make 1 h)) ) in
    help2 x ""

    let file_argument2 (x : string) : string =
      let rec help2 (x : string) (acc: string): string =
        match explode x with
        | [] -> ""
        | h::t -> (match h with
        | ' ' -> implode (t)
        | '\n' ->(implode (h::t))
        | '\"'->  (implode (h::t))
        |_ -> (if (int_of_char h) >=48 && (int_of_char h) <=57 then implode (h::t) else help2 (implode t) (acc^(String.make 1 h)) )) in
        help2 x ""
    
    
let mfile (x : string) : string =
  let rec help2 (x : string) (acc: string): string =
    match explode x with
    | [] -> ""
    | h::t -> if h = '\n' then (implode t) else help2 (implode t) (acc^(String.make 1 h)) in
    help2 (file_argument x) ""

    let getString (x : string) : const =
      let rec help2 (x : string) (acc: string): const =
    match explode x with
    | [] -> String (acc)
    | h::t -> if h = '\"' then String (acc) else help2 (implode t) (acc^(String.make 1 h)) in
    help2 (x) ""

    let isInt (x : string) : bool =
      let rec help2 (x : string) (acc: bool): bool =
        String.fold_left (fun acc x -> if (int_of_char x) <=57 && (int_of_char x) >=48 then acc else false) true x in
        match explode x with
        | [] -> false
        | h::t -> if h = '-' then help2 (implode t) true else help2 (implode (h::t)) true


  let argument (x : string) : const option=
    let rec help (x : string) (acc) : string =
      match explode x with
      | [] -> "__"
      | a::t -> if a='\"' then acc else help (implode t) (acc^(String.make 1 a)) in
      let rec help2 (x : string) (acc) : string =
        match explode x with
        | [] -> "__"
        | a::t -> if a='\n' then acc else help2 (implode t) (acc^(String.make 1 a)) in
      match explode (file_argument x) with
      | [] -> None
      | h::t -> (match h with 
      |'\"' -> (if valid_string ( (help (implode t) "")) then Some((String ( (help (implode t) "")))) else None) 
      |'\n' -> None
      |_-> if (isInt (help2 (file_argument x) "") ) then Some(Int (int_of_string (help2 (file_argument x) ""))) else None)

let argument2 (x : string) : string=
    let rec help2 (x : string) (acc) : string =
      match explode x with
      | [] -> acc
      | a::t -> if a='\n' then acc else help2 (implode t) (acc^(String.make 1 a)) in
      match explode (file_argument x) with
      | [] -> ""
      | h::t -> (match h with 
      |'\"' -> (help2 (implode t) "")
      |'\n' -> ""
      |_-> (help2 (file_argument x) ""))
      
let pop (stack : const list) : const list option=
  match stack with
  | [] -> None
  | h::t -> Some(t)

let add (stack : const list) : const list option=
match stack with
| Int(i1)::Int(i2)::t-> Some(Int(i1+i2)::t)
|_->None
let stack_tostring (x : const list) : string =
  let rec help (x : const list) (acc: string): string =
    match x with
    | [] -> acc
    | h::t -> (match h with
               | String s -> help t (acc^"\""^s^"\"")
               | Int i -> help t (acc^(string_of_int i))
               | Clo (a,b,c) -> help t (acc^(const_toString (Clo (a,b,c))))
               | Left a -> help t (acc^(const_toString (Left a)))
               | Right a -> help t (acc^(const_toString (Right a)))
               | Tuple a -> help t (acc^(const_toString (Tuple a)))
              ) in
    help x ""

let envToString (x : (string * const) list) : string =
  let rec help (x : (string * const) list) (acc: string): string =
    match x with
    | [] -> acc
    | h::t -> (match h with
               | (x, y)-> acc^"; "^x^":"^(const_toString y)) in
    help x ""

let sub (stack : const list) : const list option=
match stack with
| Int(i1)::Int(i2)::t-> Some(Int(i1-i2)::t)
| _->None

let mul (stack : const list) : const list option=
match stack with
| Int(i1)::Int(i2)::t-> Some(Int(i1*i2)::t)
| _-> None

let div (stack : const list) : const list option=
match stack with
| Int(i)::Int(0)::t->None
| Int(i1)::Int(i2)::t-> Some(Int(i1/i2)::t)
|_->None

let swap (stack : const list) : const list option=
match stack with
| [] -> None
| h::[] -> None
| a::b::t-> Some(b::a::t)

let neg (stack: const list) : const list option=
match stack with
| Int(a)::t-> Some(Int(a*(-1))::t)
| _-> None


let concat (stack: const list) : const list option=
match stack with
| String(a)::String(b)::t-> Some(String(a^b)::t)
| _ -> None

let aand (stack: const list) : const list option=
match stack with
| Int(1)::Int(1)::t-> Some(Int(1)::t)
| Int(1)::Int(0)::t-> Some(Int(0)::t)
| Int(0)::Int(1)::t-> Some(Int(0)::t)
| Int(0)::Int(0)::t-> Some(Int(0)::t)
| _ -> None

let oor (stack: const list) : const list option=
match stack with
| Int(1)::Int(1)::t-> Some(Int(1)::t)
| Int(1)::Int(0)::t-> Some(Int(1)::t)
| Int(0)::Int(1)::t-> Some(Int(1)::t)
| Int(0)::Int(0)::t-> Some(Int(0)::t)
| _ -> None

let nnot (stack: const list) : const list option=
match stack with
| Int(1)::t-> Some(Int(0)::t)
| Int(0)::t-> Some(Int(1)::t)
| _ -> None

let equal (stack: const list) : const list option=
match stack with
| Int(x)::Int(y)::t->if x=y then Some(Int(1)::t) else Some(Int(0)::t)
| _ -> None

let lte (stack: const list) : const list option=
match stack with
| Int(x)::Int(y)::t-> if x<=y then Some(Int(1)::t) else Some(Int(0)::t)
| _ -> None

let rec local (vlu: string * const) (env: (string * const) list) : (string * const) list=
match env with
| [] -> vlu::[]
| h::t -> (match h with | (x, y) -> if x= (match vlu with | (x, y) -> x) then vlu::t else h::(local vlu t))

let addEnvFun (vlu: string * const) (env: (string * const) list) : (string * const) list=
let rec localFun (vlu: string * const) (env: (string * const) list) : (string * const) list=
match env with
| [] -> []
| h::t -> (match h with | (x, y) -> if x= (match vlu with | (x, y) -> x) then t else h::(localFun vlu t)) in
match localFun vlu env with
| x-> vlu::x


let global (vlu: string * const) (env: (string * const) list) : (string * const) list=
let rec help (value: string * const) (env2: bool * ((string * const) list)) : (string * const) list =
  match env2 with 
  | (false, []) -> value::[]
  | (true, []) -> []
  | (false, h::t) -> (match h with
             | (x,y) -> if x = (match value with | (a, b) -> a) then value::(help value (true, t)) else h::(help value (false, t)) ) 
  | (true, h::t) -> h::(help value (true, t)) in
  help vlu (false, env)

let ifthen (stack: const list) : bool option=
  match stack with 
| Int(1)::t-> Some(true)
| Int(0)::t-> Some(false)
| _ -> None

(*removes the first command*)
let rec ignoreCommand (x: string): string =
  match explode x with
  | [] -> ""
  | h::t -> if h = ' ' then implode t else ignoreCommand (implode t)
(*returns only the argument*)
  let onlyArg (s: string): string = 
    let a = ignoreCommand s in 
  let rec help (x: string): string =
  match explode x with
  | [] -> ""
  | h::t -> if h = '\n' then "" else (implode (h::[])) ^help (implode t) in
  help a 
(*used for commands that don't get passed an argument, return false if there is an argument, returns true if there is no argument*)
  let noArgs (s : string): bool=
    match explode (onlyArg s) with
    | [] -> true
    | _ -> false

  (* method for determining if a parameter is a valid name*)
    let validName (s: string): string option =
      let validFirst (s: string): string option =
        match explode s with
        | [] -> None
        | h :: t ->( 
          match (int_of_char h) <= 122 && (int_of_char h) >= 97 with | true  -> Some(implode (h::[])) | _ -> None) in
      let getRest (s: string): string =
        match explode s with 
        | [] -> ""
        | h::t -> implode t in
      let rec validRest (a : string) : string option =
          match explode a with
          | [] -> Some("")
          | h :: t -> (match ((int_of_char h) <= 122 && (int_of_char h) >= 97) || ((int_of_char h) <= 57 && (int_of_char h) >= 48) || (int_of_char h = 95) ||  ((int_of_char h) <= 90 && (int_of_char h) >= 65) with | true  -> (match validRest (implode t) with | Some(x)-> Some((implode (h::[]))^x) | _ -> None) |_->None ) in
match validFirst s with 
| Some(x) -> (match validRest (getRest s) with | Some(y) -> Some(x^y) | _ -> None) 
| None-> None


(* if valid string or int argument: true means a string/int, false means name*)
  let validArg (s: string):  bool * const option =
    match ( match validName (onlyArg s) with | Some(x)-> (false, Some(x)) | _ -> (false, None) ) with 
    | (false, Some(x)) -> (false, Some( String(x)))  
    | _ -> (match argument2 (argument2 s) with | "" -> (true, argument s) | _ -> (true, None))
     
let validInt (s : string): int option =
  let rec help (s : string) : bool=
    match explode s with
    | h::t-> if (int_of_char h)<58 && (int_of_char h)>47 then help (implode t ) else false
    |[]->true in
  match argument2 (argument2 s) with
  |""->(match help (argument2 s) with |true-> Some(int_of_string (argument2 s)) |false->None)
  |_->None
(* update env*)
let rec push2 (name: string) (env: (string * const) list) (stack: const list): const list option =
  match env with 
  | [] -> None
  | h::t -> (match h with | (x, y)-> if x = name then Some(y::stack) else push2 name t stack)


(* length of cons*)
let rec length (x : 'a list) : int =
match x with
| [] -> 0
| h::t -> 1 + length t

(* returns the nth element of a list*)

(* method for using the orignal push function with a stack list *)
let pushStack (func: 'a) (x: const) (stack: const list list) : const list list =
match stack with
| [] -> (x::[])::[]
| h::t -> (func h x)::t

  let push2Stack (func: 'a) (x: const) (env: ((string * const) list) list ) (stack: const list list) : const list list option=
    let rec help (func: 'a) (x: string) (env2: ((string * const) list) list ) (stack: const list) : const list option=
      match env2 with 
      | [] -> None
      | h::t -> if (length env2)= (length env) || length t=0 then (match push2 x h stack with | None-> help func x t stack | Some(x)->Some(x)) else help func x t stack
in
match x with 
| String(x)->(
match stack with 
| [] -> (match help func x env [] with | None-> None | Some(x)-> Some(x::[]))
| h::t -> (match help func x env h with | None-> None | Some(x)-> Some(x::t)))
| _ -> None



(* passer for no-argument stack augmentors*)
let passStack (func: 'a) (stack: const list list) : const list list option=
match stack with 
| [] -> (match func [] with | None-> None | Some(x)-> Some(x::[]))
| h::t -> (match func h with | None-> None | Some(x)-> Some(x::t))

(* returns the current stack*)
let rec currentStack (stack: const list list) : const list=
  match stack with 
  | [] -> []
  | h::t -> h

(*get the local env*)
let localEnv (env: ((string * const) list) list) : (string * const) list=
  match env with 
  | [] -> []
  | h::t -> h







(* local passer*)
let localPasser (func: 'a) (vlu: string) (env: (string * const) list) (stack: const list list): (string * const) list option =
  match stack with
  | [] -> None
  | h::t -> (match h with | [] -> None | a::b -> Some( func (vlu, a) env) )
 

(*global passer*)
let globalPasser (func: 'a) (vlu: string) (env: (string * const) list list) (stack: const list list): (string * const) list list option =
let rec help (vl: string * const) (env: (string * const) list list) : (string * const) list list=
match env with
| [] -> (func vl [])::[]
| h::[]-> (func vl h)::[]
| h::t-> h::(help vl t) in
match stack with 
| [] -> None
| h::t -> (match h with | [] -> None | a::b -> Some(help (vlu, a) env) )

(*evaluate the ifthen if true remove the misc code, if false remove the misc code, check for an else  and end stateement *)
let ifTrue (file: string) : string option= 
  let rec length (x: string) : int =
    match explode x with
    | [] -> 0
    | h::t -> 1 + length (implode t) in
  let rec help (file:string) (acc: string) (rt: string ): string option =
    match acc with
    | "Else" -> Some(rt)
    |_-> if (length acc) < 4 then (match explode file with | [] -> None | h::t -> help (implode t) (acc^(implode (h::[]))) (rt^(implode (h::[]))) ) else (match explode file with | [] -> None | h::t -> (match explode acc with | a::b -> help (implode t) ((implode b)^(implode (h::[]))) (rt^(implode (h::[]))) |_ -> help (implode t) (acc^(implode (h::[]))) (rt^(implode (h::[])))))
  in 
  let rec help2 (file:string) (acc: string): string option =
    match acc with
    | "Else" -> Some(file)
    |_-> if (length acc) < 4 then (match explode file with | [] -> None | h::t -> help2 (implode t) (acc^(implode (h::[])))  ) else (match explode file with | [] -> None | h::t -> (match explode acc with | a::b -> help2 (implode t) ((implode b)^(implode (h::[]))) |_ -> help2 (implode t) (acc^(implode (h::[])))))
  in
  let rec removeElse (file: string) (acc: string) : string =
    match explode file with
    |'E'::'l'::'s'::'e'::t -> acc
    | h::t -> removeElse (implode t) (acc^(implode (h::[])))
    | [] -> acc
  in let rec getEnd (file: string) : string option =
    match explode file with
    | 'E'::'n'::'d'::t -> (match t with | [] -> Some("") | '\n'::t -> Some(implode t) | _ -> None)
    | h::t -> getEnd (implode t)
    | [] -> None in
    match (match help2 file ""  with |None->None |Some(x) -> getEnd x ) with
    | None -> None
    | Some(x)-> (match help file "" "" with | None-> None | Some(y)-> Some(mfile (removeElse y "")^x) )

  (* evaluate the ifFTHen, if false needs to cut out the ifthen-else part and just include the part until end*)
  let ifFalse (file: string) : string option= 
    let rec length (x: string) : int =
      match explode x with
      | [] -> 0
      | h::t -> 1 + length (implode t) in
      let rec help2 (file: string) (acc: string) (rt: string) : string option =
        match acc with
        | "End" -> Some(rt)
        |_-> if (length acc) < 3 then (match explode file with | [] -> None | h::t -> help2 (implode t) (acc^(implode (h::[]))) (rt^(implode (h::[]))) ) else (match explode file with | [] -> None | h::t -> (match explode acc with | a::b -> help2 (implode t) ((implode b)^(implode (h::[]))) (rt^(implode (h::[]))) |_ -> help2 (implode t) (acc^(implode (h::[]))) (rt^(implode (h::[]))))) in
    let rec help (file:string) (acc: string): string option =
      match acc with
      | "Else" -> help2 file "" ""
      |_-> if (length acc) < 4 then (match explode file with | [] -> None | h::t -> help (implode t) (acc^(implode (h::[])))  ) else (match explode file with | [] -> None | h::t -> (match explode acc with | a::b -> help (implode t) ((implode b)^(implode (h::[]))) |_ -> help (implode t) (acc^(implode (h::[])))))
    in let rec removeEnd(file: string) (acc: string) : string =
      match explode file with
      |'E'::'n'::'d'::t -> acc
      | h::t -> removeEnd(implode t) (acc^(implode (h::[])))
      | [] -> acc
    in let rec getEnd (file: string) : string option =
      match explode file with
      | 'E'::'n'::'d'::t -> (match t with | [] -> Some("") | '\n'::t -> Some(implode t) | _ -> None)
      | h::t -> getEnd (implode t)
      | [] -> None in
      match ( getEnd file) with
      | None -> None
      | Some(x)-> (match help file "" with | None-> None | Some(y)-> Some(mfile (removeEnd y "")^x) )

      let getFront (x : 'a)  =
        match x with
        | [] -> []
        | h::t -> h

        let removeFront (x:'a)=
        match x with
        | [] -> []
        | h::t -> t

      let getFront2 (x : 'a list) : 'a option=
        match x with
        | [] -> None
        | h::t -> Some(h)

        let addFront (x:'a) (y:'a list) =
          x::y

      let continue (x: int list ) : bool=
      match x with 
      | [] -> true
      | 1::t->true
      | 2::t->true
      | 4::t->true
      | 6::t->true
      | 7::t->true
      | _ -> false

      let continue2 (x: int list ) : bool=
      match x with 
      | [] -> true
      | 1::t->true
      | 2::t->true
      | 3::t->true
      | 4::t->true
      | 0::t->true
      | 6::t->true
      | 7::t->true
      | 5::t->true
      | _ -> false
let rec untilNew (x: string) : string =
  match explode x with
  | [] -> ""
  | h::t -> if h = '\n' then "" else (implode (h::[]))^(untilNew (implode t))
  
  let codeFr (code : int list): int =
    match code with
    | [] -> 0
    | h::t -> h


let addStack (x: const list list) (y: const list) =
  match x with
  | [] -> y::[]
  | h::t-> y::h::t

let removeFront2 (x: const list list) : const list list =
  match x with
  | []->[]
  | h::t-> (match h with | []->[] | a::b-> b::t)

  let injl (x: const list list) : const list list option=
  match x with
  | (h::t)::b->Some(((Left h)::t)::b)
  | _-> None
  let injr (x: const list list) : const list list option=
  match x with
  | (h::t)::b->Some(((Right h)::t)::b)
  | _-> None

  let caseLeft (stack: const list list) : ((const list list) * int )option =
    match stack with
    | []-> None
    | h::t->(match h with |a::b->(match a with |Left(a)->Some(((a::b)::t),4) |Right(a)->Some(((a::b)::t),0) |_->None)|_->None)

  let  tuple (stack: const list list) (x: int): const list list option =
    let rec help (stack: const list list) (x : int) (ret: const list ): int*const list *(const list list)=
    match stack with
    | h::t-> (match h with | a::b-> if x=1 then (1, (a::ret), (b::t) ) else help (b::t) (x-1) (a::ret)| [] -> (0, [], []))
    | [] -> (0, [], [])
  in
  match x with 
  |0->(match stack with | h::t->Some((Tuple([])::h)::t )|[]->Some((Tuple([])::[])::[]))
  |_->(
  match help stack x [] with
  | (1, h, t)-> (match t with a::b->Some((Tuple(h)::a)::b) | []->Some((Tuple(h)::[])::[]))
  | (_, _, _)->None)

  let get (stack : const list list) (x:int): const list list option=
  let rec help (x: const list) (y: int) =
    match x,y with
    | (a::b, 0)-> Some(a)
    | (a::b, _)-> help b (y-1) 
    | _->None
  in
  match stack with 
  | h::t->(match h with |a::b-> (match a with |Tuple(z)->(match (help z x) with |Some(x)->Some((x::a::b)::t) |None-> None) |_->None) |[]->None )
  |[]->None
let caseStack (stack: const list list) : const list list option=
match stack with 
| []->None
| h1::h2::t->Some((List.append h1 h2)::t)
| h::t-> Some(h::t)

let validFunArg (file:string): (string * string) option =
let rec help (file: string) (acc: string) : string option=
match explode file with
| []-> None
| ' '::t-> Some(acc)
| h::t->help (implode t) (acc^(implode (h::[]))) in
match (help (argument2 file) "") with
| Some(x)-> (match validName x with |Some(x)->(match validName (argument2 (argument2 file)) with |Some(y)-> Some(x, y)|None->None)|None->None)
| None->None

let funEnv (x:string) (y: string) ((env: ((string * const) list) list )) : (((string * const) list) list )=
match env with 
|(a)::b->(addEnvFun (x, Clo(x, y, "")) a )::b
|[]-> ((x,Clo(x,y,"") )::[])::[]

let callEnv (x:string) (y: const) ((env: ((string * const) list) list )) : (((string * const) list) list )=
match env with 
|(a)::b->(addEnvFun (x, y) a )::a::b
|[]-> ((x, y )::[])::[]


let commandArg (file: string)=
let rec help (file:string) (acc: string)=
match explode file with
| '\n'::t->Some(acc)
| h::t->help (implode t) (acc^(implode (h::[])))
| []->None in
help file ""

let funProg (env: ((string * const) list) list  ) (file: string) : (((string * const) list) list  ) option=
match env with 
| (h::t)::b-> (match h,(commandArg file) with | ((s, Clo(x,y,z)),Some(d))->Some(((s, Clo(x,y,(z^d^"\n")))::t)::b) |_->None)
|_->None

let continueFun (code: int list): bool=
match code with 
|h::t->if ((h mod 6) = 0 && h !=0 )then true else false
|_->false

let continueFun2 (code: int list): bool=
match code with 
|h::t->if (h mod 6) = 0 && h !=0 && h!=6 then true else false
|_->false

let endFun (code: int list): int list=
match code with 
| 6::t->t
| h::t-> if (h mod 6 )=0 then (h-6)::t else t
|[]->[]

let otherCode (code: int list): int list =
  match code with
  | h::t->(h+6)::t
  | []->[]

let continueReturn (code: int list)=
match code with 
| 3::t->true
| 6::t->true
| 7::t->true
| 0::t->true
| 1::t->true
| h::t -> if h mod 6=0 then true else false
|_->false

let getFront3 (x: 'a) =
  match x with
  |[]->33
  |h::t->h

  let getFront4 (stack: const list):const=
  match stack with
  | []->(String(""))
  |h::t->h

let call (stack: const list list) (file:string) (env: ((string * const) list) list ): ((const list list ) * (string ) * (((string * const) list) list ) )option=
match stack with 
| (Clo(x,y,z)::d::u)::t->Some([]::u::t, z^(mfile file), callEnv y d env)
| _->None

let interpreter (src : string) (output_file_path: string): unit =
  let rec helper (file: string) (stack : (const list) list ) (env: ((string * const) list) list ) (code: int list) : unit =
    match command file with 
    | "Push" ->  if continueFun code && (match validArg file with |(_, Some(x))->true |_->false) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path;print_string "the") else (if continue code then (match validArg file with | (true, Some(x))->  helper (mfile file) (pushStack push x stack) env code | (false, Some(x))->  (match push2Stack push2 x env stack with | Some(y)-> helper (mfile file) y env code| _ -> (quit ((String("Error")) ::[]) output_file_path);print_int (getFront3 code)) | _ -> (quit ((String("Error")) ::[]) output_file_path);print_string "abc") else helper (mfile file) stack env code);print_string (";")
    | "Pop" ->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack pop stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "pop"
    | "Add" ->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack add stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "add"
    | "Sub" ->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack sub stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "sub"
    | "Mul" ->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack mul stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "1"
    | "Div" ->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack div stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "2"
    | "Swap" ->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack swap stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "3"
    | "Neg" ->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack neg stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "4"
    | "Concat" ->if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack concat stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "5"
    | "And" ->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack aand stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "6"
    | "Or" ->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack oor stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "7"
    | "Not" ->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack nnot stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "8"
    | "Equal"-> if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack equal stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "9"
    | "Lte"->   if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match passStack lte stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "10"
    | "Local"->  if continueFun code && (match validName (onlyArg file) with | Some(x)->true |_->false) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (match validName (onlyArg file) with | Some(x)-> (match localPasser local x (getFront env) stack with | Some(y)-> helper (mfile file) (removeFront2 stack) (y::(removeFront env)) code| _ -> (quit ((String("Error")) ::[]) output_file_path) )| _ -> (quit ((String("Error")) ::[]) output_file_path)) else helper (mfile file) stack env code);print_string "11"
    | "Global"-> if continueFun code && (match validName (onlyArg file) with | Some(x)->true |_->false) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (match validName (onlyArg file) with | Some(x)->  (match globalPasser global x env stack with | Some(y)-> helper (mfile file) (removeFront2 stack) y code| _ -> quit ((String("Error")) ::[]) output_file_path)  | _ -> quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "12"
    | "IfThen"-> if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) (otherCode code) |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (if noArgs (untilNew file) then( (match ifthen (getFront stack) with | Some(x)-> if x=true then helper (mfile file) (removeFront2 stack) env (1::code) else helper (mfile file) (removeFront2 stack) env (0::code) | _ -> quit ((String("Error")) ::[]) output_file_path) )else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env (3::code) );print_string "13"
    | "Begin"->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) (otherCode code) |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (if noArgs (untilNew file) then ( (helper (mfile file) (addFront [] stack) (addFront (getFront env) env) (2::code)) )else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env (3::code));print_string "14"
    | "End"->  if continueFun2 code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) (endFun code) |None->quit ((String("Error")) ::[]) output_file_path) else  (if continue2 code then (if noArgs (untilNew file) then (( match code with | 2::t->(match (getFront2 (getFront stack )) with |None -> quit ((String("Error")) ::[]) output_file_path |Some(x)-> helper (mfile file) (pushStack push x (removeFront stack)) (removeFront env) t) |4::t-> (match caseStack stack with |Some(x)-> helper (mfile file) x env t |None->(quit ((String("Error")) ::[]) output_file_path))|h::t-> helper (mfile file) stack env t |_->(quit ((String("Error")) ::[]) output_file_path)) )else (quit ((String("Error")) ::[]) output_file_path)) else (quit ((String("Error")) ::[]) output_file_path));print_string "15"
    | "Else"->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue2 code then (if noArgs (untilNew file) then (match code with | 1::t-> helper (mfile file) stack env (0::t) | 0::t-> helper (mfile file) stack env (1::t) | 3::t-> helper (mfile file) stack env (3::t) |5::t-> helper (mfile file) stack env @@0::t |_->(quit ((String("Error")) ::[]) output_file_path) )else quit ((String("Error")) ::[]) output_file_path) else quit ((String("Error")) ::[]) output_file_path);print_int @@getFront3 code
    | "Quit" -> if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (if noArgs (untilNew file) then quit (currentStack stack) output_file_path else (quit ((String("Error")) ::[]) output_file_path)) else (helper (mfile file) stack env code));(match env with |(h::(b)::t)->print_string (envToString b) |_->print_int (5))
    | "InjL" -> if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match injl stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "18"
    | "InjR" ->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then  (if noArgs (untilNew file) then ( (match injr stack with | Some(x)-> helper (mfile file) x env code | _ -> quit ((String("Error")) ::[]) output_file_path) ) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env code);print_string "19"
    | "CaseLeft"-> if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) (otherCode code) |None->quit ((String("Error")) ::[]) output_file_path;print_int (getFront3 code)) else (if continue code then (if noArgs (untilNew file) then ( match caseLeft stack with | Some(x,y)-> helper (mfile file) x env (y::code) | None-> (quit ((String("Error")) ::[]) output_file_path);print_string (const_toString (getFront4 (getFront stack)))) else quit ((String("Error")) ::[]) output_file_path) else helper (mfile file) stack env (3::code));print_string "20"
    | "Right"-> if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue2 code then (if noArgs (untilNew file) then (match code with | 4::t-> (match caseStack stack with |Some(x)-> helper (mfile file) x env (0::t) |None->(quit ((String("Error")) ::[]) output_file_path)) |5::t-> helper (mfile file) stack env (0::t)| 0::t-> helper (mfile file) stack env (4::t) | 3::t-> helper (mfile file) stack env (3::t) |_->(quit ((String("Error")) ::[]) output_file_path) )else quit ((String("Error")) ::[]) output_file_path) else quit ((String("Error")) ::[]) output_file_path);print_string "21"
    | "Tuple"->  if continueFun code && (match validInt (file) with | Some(x)->true |_->false) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (match validInt (file) with | Some(x)-> (match tuple stack x with | Some(y)-> helper (mfile file) (y) env code| _ -> (quit ((String("Error")) ::[]) output_file_path) )| _ -> (quit ((String("Error")) ::[]) output_file_path)) else helper (mfile file) stack env code);print_string "22"
    | "Get"->   if continueFun code && (match validInt (file) with | Some(x)->true |_->false) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else (if continue code then (match validInt (file) with | Some(x)-> (match get stack x with | Some(y)-> helper (mfile file) (y) env code| _ -> (quit ((String("Error")) ::[]) output_file_path) )| _ -> (quit ((String("Error")) ::[]) output_file_path)) else helper (mfile file) stack env code);print_string "23"
    | "Fun"->  if continueFun code && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path;print_int (44)) else  (if continue code then (match validFunArg file with | Some(x,y)-> helper (mfile file) stack (funEnv x y env) (6::code)| _ -> (quit ((String("Error")) ::[]) output_file_path);print_int (66)) else (helper (mfile file) stack env (3::code);print_int (getFront3 code)))
    | "Mut"->  if (continueFun2 code) && noArgs (untilNew file) then (match funProg env file with |Some(x)-> helper (mfile file) stack (x) code |None->quit ((String("Error")) ::[]) output_file_path) else  (if (getFront2 code)=Some(6) then ((match validFunArg file with | Some(x,y)-> helper (mfile file) stack (funEnv x y env) (code)| _ -> (quit ((String("Error")) ::[]) output_file_path))) else (if getFront2 code = Some(3) then helper (mfile file) stack env (code) else (quit ((String("Error")) ::[]) output_file_path)));print_int (getFront3 code)
    | "Return"->if continueReturn code && noArgs (untilNew file) then (match getFront2 code with |Some(3)-> helper (mfile file) stack env (code) |Some(6)->(match funProg env file with |Some(x)->helper (mfile file) stack (x) code|None->quit ((String("Error")) ::[]) output_file_path)|Some(7)->(match getFront2 (getFront stack) with |Some(x)->(match removeFront stack with |h::t->(helper (mfile file) ((x::h)::t) (removeFront env) (removeFront code);print_string "ii")|[]->helper (mfile file) ((x::[])::[]) (removeFront env) (removeFront code)) |None->quit ((String("Error")) ::[]) output_file_path)|Some(0)->helper (mfile file) stack env code |Some(1)->(match getFront2 (getFront stack) with |Some(x)->(match removeFront stack with |h::t->(helper (mfile file) ((x::h)::t) (removeFront env) @@5::(removeFront code);print_string "f")|[]->helper (mfile file) ((x::[])::[]) (removeFront env) @@5::(removeFront code)) |None->quit ((String("Error")) ::[]) output_file_path)|Some(x)->if x mod 6 =0 then (match funProg env file with |Some(x)->helper (mfile file) stack (x) code|None->quit ((String("Error")) ::[]) output_file_path) else quit ((String("Error")) ::[]) output_file_path |_->quit ((String("Error")) ::[]) output_file_path) else (quit ((String("Error")) ::[]) output_file_path;print_string  (match getFront2 (getFront stack) with |Some(x)->const_toString x |_->string_of_int (getFront3 code)));print_int @@getFront3 code
    | "Call"-> if noArgs (untilNew file) then (match getFront2 code with |Some(3)-> helper (mfile file) stack env (3::code) |Some(6)->(match funProg env file with |Some(x)->helper (mfile file) stack (x) code|None->quit ((String("Error")) ::[]) output_file_path)|Some(7)->(match call stack file env with |Some(x, y,z)->helper y x z (7::code)|None->quit ((String("Error")) ::[]) output_file_path)|(Some(1))->(match call stack file env with |Some(x, y,z)->helper y x z (7::code)|None->quit ((String("Error")) ::[]) output_file_path) |(Some(2))->(match call stack file env with |Some(x, y,z)->helper y x z (7::code)|None->quit ((String("Error")) ::[]) output_file_path) |Some(4)->(match call stack file env with |Some(x, y,z)->helper y x z (7::code)|None->quit ((String("Error")) ::[]) output_file_path) |Some(0)->helper (mfile file) stack env code |Some(x)->if x mod 6 = 0 then (match funProg env file with |Some(x)->helper (mfile file) stack (x) code|None->quit ((String("Error")) ::[]) output_file_path) else quit ((String("Error")) ::[]) output_file_path |_->(if length code =0 then (match call stack file env with |Some(x, y,z)->helper y x z (7::code)|None->quit ((String("Error")) ::[]) output_file_path) else quit ((String("Error")) ::[]) output_file_path) )else quit ((String("Error")) ::[]) output_file_path;print_int @@getFront3 code
    | ""-> writeNothing output_file_path;print_string @@file^"the"
    | _ -> (quit ((String("Error")) ::[]) output_file_path);print_string "the"
  in helper src [] ([]::[]::[]) []

 