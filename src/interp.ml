open Batteries
open BatPrintf
open Location
open Symtab
module T =Typecheck
module A = Ast2
type 'a vl = 
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VNil 
  (*连接表来模仿record*)
  | VRevord of (symbol * 'a vl) list
  (*exp+body*)
  | VFunction of symbol list * A.exp
  (*数组就是ocaml的数组*)
  | VArray of 'a array

let set_record sb v l =
  try 
    List.modify sb (fun x -> v) l
  with
    Not_found -> l@[(sb,v)]

let bin_op op v1 v2 =
  match op with
  | "+" -> 
    let VInt i1 =v1 in
    let VInt i2 =v2 in
    VInt (i1+i2)
  | "+." -> 
  let VFloat i1 =v1 in
  let VFloat i2 =v2 in
  VFloat (i1+.i2)
  | "-" ->
    let VInt i1 =v1 in
    let VInt i2 =v2 in
    VInt (i1-i2)
  | "-." ->
    let VFloat i1 =v1 in
    let VFloat i2 =v2 in
    VFloat (i1-.i2)
  | "*" ->
    let VInt i1 =v1 in
    let VInt i2 =v2 in
    VInt (i1*i2)
  | "*." ->
    let VFloat i1 =v1 in
    let VFloat i2 =v2 in
    VFloat (i1*.i2)
  | "/" ->
    let VInt i1 =v1 in
    let VInt i2 =v2 in
    VInt (i1/i2)
  | "/." ->
    let VFloat i1 =v1 in
    let VFloat i2 =v2 in
    VFloat (i1/.i2)
  | "<" ->
    begin
      match v1 with
      | VInt i1 ->let VInt i2 =v2 in VBool (i1<i2)
      | VFloat i1 ->let VFloat i2 =v2 in VBool (i1<i2)
      | VString s1 -> let VString s2 = v2 in VBool(s1<s2)
    end
  | "<=" ->
    begin
      match v1 with
      | VInt i1 ->let VInt i2 =v2 in VBool (i1<=i2)
      | VFloat i1 ->let VFloat i2 =v2 in VBool (i1<=i2)
      | VString s1 -> let VString s2 = v2 in VBool(s1<=s2)
    end
  | ">" ->
    begin
      match v1 with
      | VInt i1 ->let VInt i2 =v2 in VBool (i1>i2)
      | VFloat i1 ->let VFloat i2 =v2 in VBool (i1>i2)
      | VString s1 -> let VString s2 = v2 in VBool(s1>s2)
    end
  | ">=" ->
    begin
      match v1 with
      | VInt i1 ->let VInt i2 =v2 in VBool (i1>=i2)
      | VFloat i1 ->let VFloat i2 =v2 in VBool (i1>=i2)
      | VString s1 -> let VString s2 = v2 in VBool(s1>=s2)
    end
  | "&&" ->
    let VBool b1 = v1 in
    let VBool b2 = v2 in
    VBool(b1&&b2)
  | "||" ->
    let VBool b1 = v1 in
    let VBool b2 = v2 in
    VBool(b1||b2)

let unop op v = 
  match op with
  | "not" ->
  let VBool b in VBool (not b)
  | "-" ->
  match v with
  | VInt i -> VInt (-i)
  | VFloat f -> VFloat (-.f)

let rec eval ((tenv,inloop) as env) ((raw_exp,trf)as exp) =
  match raw_exp with
  (*基本类型*)
  | A.NilExp _ -> VNil
  | A.IntExp i -> VInt i
  | A.StringEXp s -> VString s
  | A.BoolExp b -> VBool b
  | A.FloatExp f -> VFloat f

  | A.InfixOpExp (loc0,loc1,loc2) -> 
  begin 
    let op = loc0.value in
    let v1 = eval  env loc1.value in
    let v2 = eval  env loc2.value in
    match op with
    | A.Eq -> if v1 = v2 then VBool true else VBool false
    | A.Neq -> if v1= v2 then VBool false else VBool true
    | A.Plus when T.check_Int !trf ->  bin_op "+" v1 v2
    | A.Plus when T.check_Float !trf -> bin_op "+." v1 v2    
    | A.Minus when T.check_Int !trf ->  bin_op "-" v1 v2
    | A.Minus when T.check_Float !trf -> bin_op "-." v1 v2
    | A.Times when T.check_Int !trf ->  bin_op "*" v1 v2
    | A.Times when T.check_Float !trf -> bin_op "*." v1 v2
    | A.Divide when T.check_Int !trf -> bin_op "/" v1 v2
    | A.Divide when T.check_Float !trf -> bin_op "/." v1 v2
    | A.Li -> bin_op "<" v1 v2
    | A.Le -> bin_op "<+" v1 v2
    | A.Gt -> bin_op ">" v1 v2
    | A.Ge -> bin_op ">=" v1 v2
    | A.And -> bin_op "&&" v1 v2
    | A.Or -> bin_op "||" v1 v2
  end

  | A.UnaryExp (loc1,loc2) ->
  begin 
    let op = loc1.value in
    let v = eval  env loc2.value in
    match op with
    | A.Neg when T.check_Int !trf -> unop "-" v
    | A.Neg when T.check_Float !trf -> unop "-" v
    | A.Not -> unop "-" v
  end

  | A.LValueExp loc ->
    get_var env loc.value

  | A.SeqExp (explist) ->
    eval_explist env explist

  | A.Assign (loc1,loc2) ->
  begin 
    let id = loc1.value in
    let var = eval  env loc2.value in
    match id with
    | A.IdVar sb -> setref sb var tenv;VNil
    | A.SubscriptVar (loc1,loc2) ->
      let A.IdVar myarratname = loc1.value in
      let myarray = getref  myarratname tenv  in
      let e = eval  env loc2.value in
      Array.set myarray e var;VNil
    | A.FieldExp (loc,sb) ->
      let A.IdVar myrecordname = loc.value in
      let myfield = getref myrecordname tenv in
      let newfield = set_record sb var myfield in
      setref myrecordname newfield tenv;VNil

  end
  
  | A.CallExp (sb,explist) ->
    let (args,bodyexp) = getref sb tenv in
    let vallist = List.map (fun x -> eval env x) explist in
    let newtenv = List.fold_left2 (fun e sb v -> set sb v e) !tenv args vallist in
    eval (ref newtenv,inloop) bodyexp

  | A.ArrayCreate (sb,loc1,loc2) ->
    let VInt(size) = eval  env loc1.value in
    VArray (Array.make size (eval env loc2.value))

  | A.RecordCreate (sb,sbexplsit) ->
     VRevord (List.map (fun (sb,loc)-> (sb,eval env loc.value)) sbexplsit)

  | A.IfThenElse (loc0,loc1,loc2) ->
    let VBool b= eval env loc0.value in
    if b then eval env loc1.value else eval env loc2.value

  | A.IfThen (loc1,loc2) ->
    let VBool b =eval env loc1.value in
    if b then eval env loc2.value else VNil

  | A.WhileExp (loc1,loc2) ->

    let rec loop exp1 exp2 = match (eval env exp1),!inloop with
      | VBool b ,true when b -> eval env exp2
      |_ -> inloop:= false;VNil
    in inloop:=true;loop loc1.value loc2.value

  | A.ForExp (sb,_,loc0,loc1,loc2) ->
    let rec loop exp1 exp2 exp3 = match (eval env exp1),(eval env exp2),!inloop with
      | VInt i1,VInt i2,true when i1<i2 -> eval env exp3 
      | _ -> inloop:=false;VNil
    in inloop:=true;loop loc0.value loc1.value loc2.value

  | A.Break _ ->
    inloop:=false;VNil
  
  | Let (declist,loc) ->
    let newenv =
    List.fold_left (fun e dec -> eval_dec e dec) env declist in 
    eval newenv loc.value

and eval_dec ((tenv,inloop) as env) dec=
  match dec with 
  | TyDec _ -> env
  | VarDec d -> 
    let sb = d.var_name in
    let e= d.var_exp.value in
    let newtenv = set sb (eval env e) !tenv in
    (ref newtenv,inloop)
  | FunDec d -> 
    let sb = d.fun_name in
    let fieldlist = d.fun_fields|>List.map (fun (x,_,_) -> x) in
    let body = d.fun_exp.value in
    let newtenv = set sb (VFunction (fieldlist, body)) !tenv in
    (ref newtenv,inloop)

and eval_explist env explist =
  let rec loop e l =
    match l with
    | [] -> VNil
    | [x] -> eval env x
    | hd::tl -> eval env hd;loop e l

and get_var ((tenv,inloop) as env) var =
  match var with
  | A.IdVar sb -> getref sb tenv
  | A.SubscriptVar(loc1,loc2) ->
    let VArray myarray = get_var env loc1.value in
    let e = eval env loc2.value in
    Array.get myarray e
  | A.FieldExp (loc,sb) ->
    let VRevord myfield = get_var env loc.value in
    List.assoc sb myfield
  