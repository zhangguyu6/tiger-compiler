open Batteries
open BatPrintf
open Env
open Location
module A = Ast2

module S = Symtab
module E = Errors
module T = Types

(*检查类型是否相符,否则抛出错误*)
let tymatch (ty1:T.ty) (ty2:T.ty) (pos:t)= 
  if T.check_ty ty1 ty2 then ty1
  else 
  let sty1 = T.string_of_ty ty1 in
  let sty2 = T.string_of_ty ty2 in
  E.raise_TypeCheckError (sprintf " dismatch type %s:%s" sty1 sty2) pos

(*从类型环境中查找相关量*)
let tylook (tenv:tenv) (id:S.symbol)  (pos:t) :(T.ty) =
  match S.get id tenv with
  | Some ty -> ty
  | None -> printf "undefined type %s at %s" (S.to_string id) (string_of_loc pos);T.Nil
 
let tylookorfail (tenv:tenv) (id:S.symbol) :(T.ty) =
  match S.get id tenv with
  | Some ty -> ty
  | None -> failwith "cann't find type var" 

let set_type trf ty = 
  trf:=ty;ty
(*基本类型的检查*)
let check_Int ty =T.check_ty T.Int ty

let check_Float ty=T.check_ty T.Float ty

let check_Bool ty=T.check_ty T.Bool ty

let check_String ty=T.check_ty T.String ty

let check_Nil ty=T.check_ty T.Nil ty

let rec check_exp ((tenv,venv,in_loop) as env) (raw_exp,trf) =
  match raw_exp with 
  (*基本类型*)
  | A.NilExp _ -> set_type trf T.Nil
  | A.IntEXp _ -> set_type trf T.Int
  | A.StringEXp _ -> set_type trf T.String
  | A.BoolExp _ -> set_type trf T.Bool
  | A.FloatExp _ -> set_type trf T.Float

  | A.InfixOpExp (loc0,loc1,loc2) ->
  begin
    let op = loc0.value in
    let tyl = check_exp env loc1.value in
    let tyr = check_exp env loc2.value in
    match op with
    | A.Eq | A.Neq ->
    tymatch tyl tyr loc0.loc
    | A.Plus | A.Minus | A.Times | A.Divide |A.Li | A.Le | A.Gt | A.Ge  ->
    if check_Int tyl then tymatch tyl tyr loc0.loc
    else if check_Float tyl then tymatch tyl tyr loc0.loc
    else E.raise_TypeCheckError 
    (sprintf " %s : %s is not int or float" (T.string_of_ty tyl) (T.string_of_ty tyr)) loc0.loc

    | A.And | A.Or  ->
    if check_Bool tyl then tymatch tyl tyr loc0.loc
    else E.raise_TypeCheckError 
    (sprintf " %s : %s is not bool" (T.string_of_ty tyl) (T.string_of_ty tyr)) loc0.loc
  end

  | A.UnaryExp (loc1,loc2) ->
  begin
    let op = loc1.value in
    let ty = check_exp env loc2.value in
    match op with
    | A.Neg ->
    if check_Int ty then tymatch ty T.Int loc1.loc
    else if check_Float ty then tymatch ty T.Float loc1.loc
    else E.raise_TypeCheckError  (sprintf "%s is not int or float"  (T.string_of_ty ty))  loc1.loc

    | A.Not ->
    if check_Bool ty then tymatch ty T.Bool loc1.loc
    else E.raise_TypeCheckError (sprintf "%s  not int or float" (T.string_of_ty ty)) loc1.loc
  end

  (*引用类型*)
  | A.LValueExp loc ->
    set_type trf (check_var loc.value loc.loc env)
  
  (*求值表达式列表,返回最后一个表达式的类型*)
  | A.SeqExp exp_loc_list ->
    check_exp_loc_list env exp_loc_list
  | A.CallExp (sb,loc) -> 
    begin
      let args = 
        loc |>
        List.map (fun e -> check_exp env (e.value)) in
        match S.get sb tenv with 
        | Some (T.Function (tylist,ty)) -> 
          if T.check_tylist args tylist then set_type trf ty
          else failwith "callexp dismatch"
          
        | _ -> failwith "callexp dismatch"
    end

  | A.Assign (loc1,loc2) ->
    let varty =  check_var loc1.value loc1.loc env in
    let expty =  check_exp env loc2.value in
    tymatch varty expty loc1.loc
  
  | A.RecordCreate (sb,sb_loclist) ->
    begin
      let expectedty = tylookorfail tenv sb in
      match expectedty with
      | T.Record (sb_tylist,_) ->
        if check_record sb_tylist sb_loclist env then expectedty
        else failwith "recordcreate dismatch"
      | _ -> failwith "recordcreate dismatch"
    end

  | A.ArrayCreate (sb,loc1,loc2) ->
  begin
    let ty = tylookorfail tenv sb in
    let sizety = check_exp env loc1.value in
    let initty = check_exp env loc2.value in
    if check_Int sizety then tymatch ty initty loc1.loc
    else E.raise_TypeCheckError "ArrayCreate dismatch" loc1.loc
  end
  
  | A.IfThenElse (loc0,loc1,loc2) ->
    let ifty = check_exp env loc0.value in
    let thenty = check_exp env loc1.value in
    let elsety = check_exp env loc2.value in
    if check_Bool ifty 
      then
        begin
          if T.check_ty thenty elsety 
          then set_type trf thenty
          else E.raise_TypeCheckError "IfThenElse dismatch" loc1.loc
        end
      else E.raise_TypeCheckError "Ifcond dismatch" loc0.loc

  | A.IfThen (loc1,loc2) ->
    let ifty = check_exp env loc1.value in
    let thenty = check_exp env loc2.value in
    if check_Bool ifty 
    then set_type trf thenty
    else E.raise_TypeCheckError "IfThen dismatch" loc1.loc

  | A.WhileExp (loc1,loc2) ->
    let whilety = check_exp (tenv,venv,true) loc1.value in
    if check_Bool whilety 
    then set_type trf T.Nil
    else E.raise_TypeCheckError "WhileExp dismatch" loc1.loc

  | A.ForExp (sb,escape,loc0,loc1,loc2) ->
    let initty = check_exp env loc0.value in
    let endty = check_exp env loc1.value in
    let doexp = loc2.value in
    if check_Int initty && check_Int endty
    then
      let newvenv = S.set sb (VarEntry T.Int) venv in  

      ignore (check_exp (tenv,newvenv,true) doexp);
      set_type trf T.Nil
    else
      E.raise_TypeCheckError "ForExp dismatch" loc0.loc

  | A.Break loc ->
    if  in_loop then set_type trf T.Nil
    else 
      E.raise_TypeCheckError "Break out of loop" loc.loc

  | A.Let (declist,loc) -> 
    let newenv = check_declist env declist in
    let expty = check_exp newenv loc.value in
    set_type trf expty

and check_var var loc ((tenv,venv,in_loop) as env)= match var with
    | A.IdVar sb ->
    begin
      match S.get sb venv with 
      | Some (VarEntry ty) -> ty
      | _ -> E.raise_TypeCheckError "not a var" loc
    end
    | A.SubscriptVar (loc1,loc2) ->
      let varty = check_var loc1.value loc1.loc env in
      let expty =  check_exp env loc2.value in
      if check_Int expty 
      then
        match varty with
        | T.Array _ as ty -> ty
        | _ -> E.raise_TypeCheckError "not a array typr" loc
      else E.raise_TypeCheckError "not a subscrip is not int" loc
    | A.FieldExp (loc,sb) ->
      let varty = check_var loc.value loc.loc env in
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
  | [exp_loc] -> check_exp env (exp_loc.value)
  | hd::tl -> ignore (check_exp env hd.value); check_exp_loc_list env tl

and check_record sb_tylist sb_loclist env =
    let actualtylsit = sb_loclist |>
        List.map (fun (_,loc) -> check_exp env (loc.value)) in
    let expectedtylist = sb_tylist |>
        List.map (fun (_,ty) -> ty) in
        T.check_tylist  actualtylsit expectedtylist

and check_declist env declist =
  declist |>
  List.fold_left (fun env dec->check_dec env dec) env 

and check_dec ((tenv,venv,in_loop) as env) dec =
  match dec with
  | A.TyDec {type_name;type_type} ->
    let ty = tranFromAtytoTty tenv type_type in
    let newtenv = S.set type_name ty tenv in
    (newtenv,venv,in_loop)

  | A.VarDec {var_name;
              var_type;
              var_exp;
            var_escape}  ->
    let actualty = check_exp env var_exp.value in
    let expectedty = tylookorfail  tenv var_name in
    if T.check_ty actualty expectedty 
    then 
      begin 
         let newvenv = S.set var_name (VarEntry actualty) venv in
         (tenv,newvenv,in_loop)
      end
    else E.raise_TypeCheckError "id dismatched tyid" var_exp.loc

  | A.FunDec {fun_name;
              fun_type;
              fun_fields;
              fun_exp} -> 
    let actualty = check_exp env fun_exp.value in
    let expectedty = tylookorfail  tenv fun_name in
    let tylist= tranFieldlsit_to_tylist fun_fields tenv in
    if T.check_ty actualty expectedty 
    then 
      begin
        let newvenv =S.set fun_name (FunEntry(tylist,actualty)) venv in
         (tenv,newvenv,in_loop)
      end
    else E.raise_TypeCheckError "FunDec dismatched " fun_exp.loc

and tranFromAtytoTty tenv aty  = match aty with
  | A.Tyid sb -> tylookorfail tenv sb 
  | A.RecTy fieldlist -> tranFromfieldlist tenv fieldlist 
  | A.ArrTy (loc) -> 
    let ty = tylookorfail tenv loc.value in
    T.Array (ty,T.incridex ())

and tranFromfieldlist tenv fieldlist = 
  let sb_tylist =
  List.map (fun x ->
    let field = x.value in
    let (id,_,tyid) = field in
    let ty = tylookorfail tenv tyid  in
    (id,ty)) fieldlist  in
  T.Record(sb_tylist,T.incridex ())

and tranFieldlsit_to_tylist fieldlist tenv=
  fieldlist |>
  List.map (fun x ->
    let field = x .value in
    let (_,_,tyid) = field in tylookorfail  tenv tyid)