open Batteries
open BatPrintf
module A = Ast
module L = Location
module S = Symtab
module E = Errors
module T = Env
(*值环境*)
type venv = T.entry S.t
(*类型环境*)
type tenv = T.ty S.t


let eval_env name inf env  =
  match (S.get (inf.value |> S.to_string) env) with
  | Some v -> v
  | _ ->  E.raise_NameError inf.loc name

let eval_venv = eval_env "value" 
let eval_tenv = eval_env "type" 
let eval_funenv = eval_env "function"

let rec check_exp venv tenv exp =
  match exp with
  | A.LValueExp inf -> check_var venv tenv inf.value
  | A.NilExp _ -> T.TNil
  | A.IntEXp _ -> T.TInt
  | A.StringEXp -> T.TString
  | A.BoolExp -> T.TBool
  | A.FloatExp -> T.TFloat
  (*序列表达式的类型,是最后一个表达式的类型*)
  | A.SeqExp infolist -> 
    infolist |> 
    List.map (fun x -> x.value) |>
    List.fold_left (fun _ exp -> check_exp venv tenv exp) T.TNil

  | A.InfixOpExp (inf1,inf2,inf3) ->
    check_binop inf1 (check_exp inf2.value) (check_exp inf3.value)

  | A.UnaryExp (inf1,inf2) ->
    check_unop inf1 (check_exp inf2.value)

  | A.CallExp (inf1,inf2) -> 
    let funname = inf1.value |> S.to_string in
    let explist = inf2 |> List.map (fun x -> x.value) in 
    check_callexp funname explist inf1 venv tenv

  | A.ArrayCreate (inf1,inf2,inf3) -> 
    let T.TArray(ty,_) = eval_tenv inf1.value tenv in
    let tyinit = check_exp venv tenv inf3.value in
    if T.check_ty ty tyinit then ty
    else E.raise_TypeError inf1.loc "excepted init dismatched inittype"

  | A.RecordCreate (inf1,fields) ->
    let ty =  eval_tenv inf1.value tenv
    match ty with
    |T.TRecord (excepttylist,_) ->
    let tylist = excepttylist |> List.map snd in
    let actlist = fields |> List.map (fun (_,e) -> check_exp e.value) in
    if T.check_tylist tylist actlist then ty
    else E.raise_TypeError inf1.loc "recordfield dismatched"

  | A.Assign (inf1,inf2) ->
    let ty = check_var venv tenv inf1.value in
    let actty = check_exp venv tenv inf2.value in 
    if T.check_tylist ty actty then ty
    else E.raise_TypeError inf1.loc "Assign dismatched"

  | A.IfThenElse (inf1,inf2,inf3) ->
    let thenty = check_exp venv tenv inf2.value in 
    let elsety = check_exp venv tenv inf3.value in 
    if (T.check_ty (check_exp venv tenv inf1.value) T.TBool) &&(T.check_ty thenty elsety)
    then thenty
    else E.raise_TypeError inf1.loc "ifthenelse dismatched"

  | A.IfThen (inf1,inf2) ->
    let ifty = check_exp venv tenv inf1.value in
    let thenty = check_exp venv tenv inf2.value in 
    if (T.check_ty ifty T.TBool) 
    then thenty
    else E.raise_TypeError inf1.loc "ifthen dismatched"

  | A.WhileExp (inf1,inf2) ->
    let whilety = check_exp venv tenv inf1.value in
    let doty = check_exp venv tenv inf2.value in
    if (T.check_ty whilety T.TBool) && (T.check_ty doty T.TNil)
    then doty
    else E.raise_TypeError inf1.loc "WhileExp dismatched"
  
  | A.ForExp (_,_,from,toinf,doinf) ->
    let fromty = check_exp venv tenv from.value in
    let toty = check_exp venv tenv toinf.value in
    let doty = check_exp venv tenv doinf.value in
    if (T.check_ty fromty T.TInt) && (T.check_ty toty T.TInt) && (T.check_ty doinf T.TNil)
    then T.TNil
    else E.raise_TypeError from.loc "ForExp dismatched"
  
  | A.Break _ -> T.TNil

  | A.Let (declist,inf) ->
    let (newvenv,newtenv) = check_declist declist venv tenv in
    check_exp newvenv newtenv inf.value

let check_declist venv tenv declist =
    declist |>
    List.fold_left (fun (t,v) x -> check_dec t v x) (venv,tenv) 

let check_dec venv tenv = function 

  | A.TyDec {type_name;type_type} ->
    let tyid =  type_name.value  |> S.to_string in
    let ty = check_ty tenv type_type in
    (venv,S.set tyid ty tenv)

  | A.VarDec {var_name;var_type;var_exp;_} ->
    let varid = var_name.value |> S.to_string
    let ty = eval_tenv var_type tenv in
    let expty = check_exp venv tenv var_exp in
    if T.check_ty ty expty 
    then 
    (S.set varid (T.VarEntry ty) venv,tenv)
    else E.raise_TypeError var_name.loc "VarDec dismatched"

  | A.FunDec {fun_name;fun_type;fun_fields;fun_exp} ->
    let funid = fun_name.value |> S.to_string in
    let returnty = eval_tenv tenv fun_type in
    let actualty = check_exp venv tenv fun_exp.value
    let tylist = fun_fields |>
    List.map (fun field -> eval_tenv field.field_type.value tenv) in
    if T.check_ty returnty actualty
    then 
    (S.set funid (T.FunEntry(tylist,returnty)) venv,tenv)
    else E.raise_TypeError fun_name.loc "FunDec dismatched"



let check_ty venv tenv= function
  | A.Tyid inf -> eval_tenv inf tenv
  | A.RecTy fieldlist -> 
    let ftylist =
    List.map (fun field -> (field.field_name.value,eval_tenv field.field_type.value tenv)) fieldlist in
    T.TRecord (ftylist,U.incridex ())
  | A.ArrTy inf ->T.TArray(eval_tenv inf tenv,U.incridex ())

let check_callexp funname explist funinf venv tenv=
    match (S.get funname venv) with
    | Some (T.VarEntry _) -> 
      E.raise_TypeError funinf.loc "excepted funvar"
    | Some (T.FunEntry (tylist,ty)) -> 
      let actualtylist = explist |> List.map (fun e ->check_exp venv tenv e) in
      T.check_tylist actualtylist tylist;ty
    | None ->
      E.raise_TypeError funinf.loc "cann't find funvar"


let check_unop opinf ty1 = match opinf.value with
  | A.Neg -> 
  if is_arith_ty ty1 then ty1 
  else E.raise_TypeError opinf.loc "not int or float"
  | A.Not ->
  if ty1 = T.TBool then T.TBool 
  else E.raise_TypeError opinf.loc "not a bool type"

let check_binop opinf ty1 ty2 = match opinf.value with
  | A.Plus | A.Minus | A.Times | A.Divide ->
   if ty1 = ty2 && is_arith_ty ty1 then ty1
   else E.raise_TypeError opinf.loc "left and right not match"
  | A.Li | A.Le| A.Gt | A.Ge| A.And| A.Or ->
   if ty1 = ty2 && is_logic_ty ty1 then T.TBool
   else E.raise_TypeError opinf.loc "left and right not match"
  | A.Eq -> 
   if ty1 = ty2 && is_arith_ty ty1 then T.TBool
   else E.raise_TypeError opinf.loc "left and right not match"
   


let is_arith_ty = function
  | T.TInt | T.TFloat -> true
  | _ ->false

let is_logic_ty = function
  | T.TBool | T.TInt | T.TFloat -> true
  | _ -> false

let rec check_var venv tenv var =
  match var with
  | A.IdVar inf -> 
    begin
      match eval_venv  inf venv with
      | T.VarEntry ty -> ty
      | T.FunEntry _ -> 
        E.raise_TypeError inf.loc "excepted a idvar"
    end
  | A.SubscriptVar (inf,_) ->
     match check_var venv tenv inf.value with
     | TArray (ty,_) -> ty
     | _ -> E.raise_TypeError inf.loc "excepted a array"
  | A.FieldExp (inf1,inf2) ->
     match check_var venv tenv inf.value with
     | T.TRecord (fields,_) ->
      begin
        try
          List.assoc inf2.value fields
        with 
          Not_found -> E.raise_NameError inf2.loc "cann't find related id"
      end
     | _ -> E.raise_TypeError inf1.loc "excepted a Record"
  | _ -> failwith "unknown type"