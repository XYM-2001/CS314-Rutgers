open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec var_helper x (env : environment) =
  match env with
  |[] -> raise UndefinedVar
  |(a,b)::t -> if a = x then b else var_helper x t ;; 
let rec env_helper x v env =
  match env with
  |(a,b)::t -> if a = x then (a,v)::t else (a,b)::(env_helper x v t)
  |[] -> [(x,v)];;
let rec assg_helper x v env = 
  match env with
  |(a,b)::t -> 
      if a = x then 
        match b,v with
        |Int_Val f,Int_Val s -> (x,Int_Val s)::t
        |Bool_Val f,Bool_Val s -> (x,Bool_Val s)::t
        |Closure (f1,f2,f3),Closure (s1,s2,s3) -> (x,Closure (s1,s2,s3))::t
        |_,_ -> raise TypeError
      else 
        (a,b)::(assg_helper x v t)
  |[] -> raise UndefinedVar;; 

let rec eval_expr (e : exp) (env : environment) : value =
  match e with
  |Number x -> Int_Val x
  |True -> Bool_Val true
  |False -> Bool_Val false
  |Var x -> var_helper x env
  |Minus (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x, Int_Val y -> Int_Val(x-y)
       |_,_ -> raise TypeError)
  |Plus (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> Int_Val(x+y) 
       |_,_ -> raise TypeError) 
  |Times (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> Int_Val(x*y) 
       |_,_ -> raise TypeError) 
  |Div (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> if (eval_expr (Eq (e2,(Number 0))) env) = Bool_Val true then raise DivByZeroError else Int_Val(x/y) 
       |_,_ -> raise TypeError) 
  |Mod (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> if (eval_expr (Eq (e2,(Number 0))) env) = Bool_Val true then raise DivByZeroError else Int_Val(x mod y)
       |_,_ -> raise TypeError)
  |And (e1,e2) ->
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Bool_Val x,Bool_Val y -> if x && y then Bool_Val true else Bool_Val false
       |_,_ -> raise TypeError)
  |Or (e1,e2) ->
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Bool_Val x,Bool_Val y -> if x || y then Bool_Val true else Bool_Val false
       |_,_ -> raise TypeError)
  |Not e -> 
      let n = eval_expr e env in
      (match n with
       |Bool_Val x -> Bool_Val (not x)
       |_ -> raise TypeError)
  |Lt (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> if x < y then Bool_Val true else Bool_Val false
       |_,_ -> raise TypeError)
  |Leq (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> if x <= y then Bool_Val true else Bool_Val false
       |_,_ -> raise TypeError)
  |Eq (e1,e2) -> 
      let n1 = eval_expr e1 env in
      let n2 = eval_expr e2 env in
      (match n1,n2 with
       |Int_Val x,Int_Val y -> if x = y then Bool_Val true else Bool_Val false
       |Bool_Val x, Bool_Val y -> if x = y then Bool_Val true else Bool_Val false 
       |_,_ -> raise TypeError)
  |App (e1,e2) -> 
      let c = eval_expr e1 env in 
      let v = eval_expr e2 env in
      (match c with
       |Closure (environment,x, e) -> 
           let new_env = env_helper x v environment in
           eval_expr e new_env
       |_ -> raise TypeError) 
  |Fun (x,e) -> Closure (env, x, e);; 

(* evaluate a command in an environment *) 
let rec eval_command (c : com) (env : environment) : environment =
  match c with
  |Skip -> env
  |Comp (c1,c2) ->
      let new_env = eval_command c1 env in
      eval_command c2 new_env
  |Declare (t,s) -> 
      (match t with
       |Int_Type -> env @ [(s, Int_Val 0)]
       |Bool_Type -> env @ [(s,Bool_Val false)]
       |Lambda_Type -> env @ [(s,Closure (env, "x", Var "x"))])
  |Assg (s,e) -> 
      let newv = eval_expr e env in
      assg_helper s newv env
  |Cond (e, c1, c2) -> 
      let b = eval_expr e env in
      (match b with
       |Bool_Val x -> if x = true then eval_command c1 env else eval_command c2 env
       |_ -> raise TypeError)
  |While (e,c1) -> 
      let b = eval_expr e env in
      (match b with
       |Bool_Val x -> if x = true then eval_command c (eval_command c1 env) else env
       |_ -> raise TypeError)
  |For (e, c1) -> 
      let n = eval_expr e env in
      if n = Int_Val 0 then env
      else eval_command (For ((Minus(e,Number 1)),c1)) (eval_command c1 env) ;; 