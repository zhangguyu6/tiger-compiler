open Batteries
open BatPrintf
open Env
module A = Ast2
module L = Location
module S = Symtab
module E = Errors
module T = Types

(*检查类型是否相符,否则抛出错误*)
let tymatch (ty1:T.ty) (ty2:T.ty) (pos:L.t)= 
  if T.check_ty ty1 ty2 then ty1
  else 
  let sty1 = T.string_of_ty ty1 in
  let sty2 = T.string_of_ty ty2 in
  E.raise_TypeCheckError (sprintf " dismatch type %s:%s" sty1 sty2) pos

(*从类型环境中查找相关量*)
let tylook (tenv:tenv) (id:S.symbol)  (pos:L.t) :(T.ty) =
  match S.get id tenv with
  | Some ty -> ty
  | None -> printf "undefined type %s at %s" (S.to_string id) (L.string_of_loc pos);T.Nil
 
let tylookorfail (tenv:tenv) (id:S.symbol) :(T.ty) =
  match S.get id tenv with
  | Some ty -> ty
  | None -> failwith "cann't find type var" 

let set_type trf ty = 
  trf:=ty;ty
(*基本类型的检查*)
let check_Int ty=T.check_ty T.Int
let check_Float ty=T.check_ty T.Float
let check_Bool ty=T.check_ty T.Bool
let check_String ty=T.check_ty T.String
let check_Nil ty=T.check_ty T.Nil

let rec check_exp ((tenv,venv,in_loop) as env) ((raw_exp,trf) as exp) =
  match raw_exp with 
  (*基本类型*)
  | A.NilExp _ -> set_type trf T.Nil
  | A.IntEXp _ -> set_type trf T.Int
  | A.StringEXp _ -> set_type trf T.String
  | A.BoolExp _ -> set_type trf T.Bool
  | A.FloatExp _ -> set_type trf T.Float

  (*基本组合类型*)
  | A.InfixOpExp (loc0,loc1,loc2) ->
    let op = loc0.value in
    let tyl = check_exp env loc1.value in
    let tyr = check_exp env loc2.value in
    match op with
    | A.Eq | A.Neq ->
    tymatch tyl tyr op.loc
    | A.Plus | A.Minus | A.Times | A.Divide |A.Li | A.Le | A.Gt | A.Ge  ->
    if check_Int tyl then tymatch tyl tyr op.loc
    else if check_Float tyl then tymatch tyl tyr op.loc
    else E.raise_TypeCheckError 
    (sprintf " %s : %s is not int or float" (T.string_of_ty tyl) (T.string_of_ty tyr) op.loc)

    | A.And | A.Or  ->
    if check_Bool tyl then tymatch tyl tyr op.loc
    else E.raise_TypeCheckError 
    (sprintf " %s : %s is not bool" (T.string_of_ty tyl) (T.string_of_ty tyr) op.loc)

  | A.UnaryExp (loc1,loc2) ->
    let op = loc1.value in
    let ty = check_exp env loc2.value in
    match op with
    | A.Neg ->
    if check_Int ty then tymatch ty T.Int op.loc
    else if check_Float ty then tymatch ty T.Float op.loc
    else E.raise_TypeCheckError 
    (sprintf " %s is not int or float" (T.string_of_ty ty)) op.loc

    | A.Not ->
    if check_Bool ty then tymatch ty T.Bool op.loc
    else E.raise_TypeCheckError (sprintf " %s is not int or float"  (T.string_of_ty ty) ) op.loc
  
  (*引用类型*)
  | A.LValueExp loc ->
    set_type trf (check_var loc.value loc.loc)
  
  (*求值表达式列表,返回最后一个表达式的类型*)
  | A.SeqExp exp_loc_list ->
    check_exp_loc_list env exp_loc_list
  | A.CallExp (sb,loc) -> 
    begin
      let args = 
        loc |>
        List.map (fun e -> e.value) in
        match S.get sb tenv with 
        | Some (T.Function (tylist,ty)) -> 
          if T.check_tylist args tylist then set_type trf ty
          else E.raise_TypeCheckError 
          (sprintf "fun args %s:%s " (T.string_of_tylist args) (T.string_of_tylist tylist) loc.loc)
        | _ -> failwith "callexp dismatch"
    end

  | A.Assign (loc1,loc2) ->
    let varty =  check_var loc1.value loc1.loc in
    let expty =  check_exp env loc2.value in
    tymatch varty expty loc1.loc
  
  | A.RecordCreate (sb,sb_loclist) ->
    let expectedty = tylookorfail tenv sb in
    match expectedty with
    | T.Record (sb_tylist,_) ->
      if check_record sb_tylist sb_loclist env then expectedty
      else failwith "recordcreate dismatch"
    | _ -> failwith "recordcreate dismatch"

  | A.ArrayCreate (sb,loc1,loc2) ->
    let ty = tylookorfail tenv sb in
    let sizety = check_exp env loc1.value in
    let initty = check_exp env loc2.value in
    if check_Int sizety then tymatch ty initty
    else E.raise_TypeCheckError "ArrayCreate dismatch" loc1.loc
  
  | 

and check_var var loc  = match var with
    | A.IdVar sb ->
    begin
      match S.get sb venv with 
      | Some (E.VarEntry ty) -> ty
      | _ -> E.raise_TypeCheckError "not a var" loc
    end
    | A.SubscriptVar (loc1,loc2) ->
      let varty = check_var loc1.value loc1.loc in
      let expty =  check_exp env loc2.value in
      if check_Int expty 
      then
        match S.get sb tenv with
        | T.Array _ as ty -> ty
        | _ -> E.raise_TypeCheckError "not a array typr" loc
      else E.raise_TypeCheckError "not a subscrip is not int" loc
    | A.FieldExp (loc,sb) ->
      let varty = check_var loc.value loc.loc in
      match varty with 
      | T.Record (sb_tylist,_) ->
        begin 
          try 
            List.assoc sb sb_tylist 
          with 
            Not_found -> E.raise_TypeCheckError "not a array typr" loc.loc
        end
      | _ ->  E.raise_TypeCheckError "not a record typr" loc.loc

and check_exp_loc_list env = function
  | [] -> T.Nil
  | [exp_loc] -> check_exp env (exp_loc |> L.extract_value)
  | hd::tl -> check_exp env hd.value; check_exp_loc_list env tl

and check_record sb_tylist sb_loclist env =
    let actualtylsit = sb_loclist |>
        List.map (fun (_,loc) -> check_exp env loc.value) in
    let expectedtylist = sb_tylist |>
        List.map (fun (_,ty) -> ty) in
        T.check_tylist  actualtylsit expectedtylist
  