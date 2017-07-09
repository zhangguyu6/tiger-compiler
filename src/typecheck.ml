open Batteries
open BatPrintf
open Env
open Location
module A = Ast
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

let rec check_exp ({v=venv;t=tenv;inloop = inloop} as env) ((exploc,trf) as exp) =
  match exploc.value with 
  (*基本类型*)
  | A.NilExp _ -> set_type trf T.Nil
  | A.IntExp _ -> set_type trf T.Int
  | A.StringEXp _ -> set_type trf T.String
  | A.BoolExp _ -> set_type trf T.Bool
  | A.FloatExp _ -> set_type trf T.Float

  | A.InfixOpExp (op,e1,e2) ->
  begin
    let tyl = check_exp env e1 in
    let tyr = check_exp env e2 in
    match op with
    | A.Eq | A.Neq ->
    tymatch tyl tyr exploc.loc

    | A.Plus | A.Minus | A.Times | A.Divide |A.Li | A.Le | A.Gt | A.Ge  ->
    if check_Int tyl then tymatch tyl tyr exploc.loc
    else if check_Float tyl then tymatch tyl tyr exploc.loc
    else E.raise_TypeCheckError 
    (sprintf " %s : %s is not int or float" 
    (T.string_of_ty tyl) (T.string_of_ty tyr)) exploc.loc

    | A.And | A.Or  ->
    if check_Bool tyl then tymatch tyl tyr exploc.loc
    else E.raise_TypeCheckError 
    (sprintf " %s : %s is not bool" (T.string_of_ty tyl) (T.string_of_ty tyr)) exploc.loc
  end

  | A.UnaryExp (op,e) ->
  begin
    let ty = check_exp env e in
    match op with
    | A.Neg ->
    if check_Int ty then tymatch ty T.Int  exploc.loc
    else if check_Float ty then tymatch ty T.Float  exploc.loc
    else E.raise_TypeCheckError  
    (sprintf "%s is not int or float"  (T.string_of_ty ty))  
     exploc.loc

    | A.Not ->
    if check_Bool ty then tymatch ty T.Bool  exploc.loc
    else E.raise_TypeCheckError 
    (sprintf "%s  not int or float" (T.string_of_ty ty)) 
     exploc.loc
  end

  (*引用类型*)
  | A.LValueExp var ->
    set_type trf (check_var var exploc.loc env)
  
  (*求值表达式列表,返回最后一个表达式的类型*)
  | A.SeqExp exp_loc_list ->
    check_exp_loc_list env exp_loc_list

  | A.CallExp (sb,loc) -> 
    begin
      let args = 
        loc |>
        List.map (fun e -> check_exp env e) in
      
        (*先找到函数的类型声明*)
        match S.get sb tenv with 
        | Some (T.Function (expargs,return)) -> 
          (*当函数参数类型与传入参数类型一致时,将调用设置为返回类型*)
          if T.check_tylist args expargs then set_type trf return
          else failwith "callexp dismatch"
          
        | _ -> failwith "callexp dismatch"
    end

  | A.Assign (var,e) ->
  (*赋值是对已声明的变量的值的修改,赋值不改变变量的类型,因此这里只检查声明类型和表达式类型是否相符*)
    let varty =  check_var var exploc.loc env in
    let expty =  check_exp env e in
    tymatch varty expty exploc.loc
  
  | A.RecordCreate (sb,sb_loclist) ->
  (*记录构造是调用已有的记录类型生成新的记录实例,记录构造不改变类型*)
    begin
      let expectedty = tylook tenv sb exploc.loc in
      match expectedty with
      | T.Record (sb_tylist,_) ->
        if check_record sb_tylist sb_loclist env then expectedty
        else failwith "recordcreate dismatch"
      | _ -> failwith "recordcreate dismatch"
    end

  | A.ArrayCreate (sb,e1,e2) ->
  (*数组构造是调用已有的数组类型生成新的数组实例,数组构造不改变类型*)
  begin
    let ty = tylook tenv sb exploc.loc in
    let sizety = check_exp env e1 in
    let initty = check_exp env e2 in
    if check_Int sizety then tymatch ty initty exploc.loc
    else E.raise_TypeCheckError "ArrayCreate dismatch" exploc.loc
  end
  
  | A.IfThenElse (e1,e2,e3) ->
    let ifty = check_exp env e1 in
    let thenty = check_exp env e2 in
    let elsety = check_exp env e3 in
    if check_Bool ifty 
      then
        begin
          if T.check_ty thenty elsety 
          then set_type trf thenty
          else E.raise_TypeCheckError "IfThenElse dismatch" exploc.loc
        end
      else E.raise_TypeCheckError "Ifcond dismatch" exploc.loc

  | A.IfThen (e1,e2) ->
    let ifty = check_exp env e1 in
    let thenty = check_exp env e2 in
    if check_Bool ifty 
    then set_type trf thenty
    else E.raise_TypeCheckError "IfThen dismatch" exploc.loc

  | A.WhileExp (e1,e2) ->
    let whilety = check_exp env e2 in
    if check_Bool whilety 
    then set_type trf T.Nil
    else E.raise_TypeCheckError "WhileExp dismatch" exploc.loc

  | A.ForExp (sb,escape,e1,e2,e3) ->
    (*for可以视做let,for语句声明并修改了一个循环变量*)
    let initty = check_exp env e1 in
    let endty = check_exp env e2 in
    if check_Int initty && check_Int endty
    then
    (*绑定新变量到do的作用域中,同时使inloop=true*)
      let newenv = add_venv sb initty env in  
      let newenv =  {newenv with inloop = true} in
      ignore (check_exp newenv e3);
      set_type trf T.Nil
    else
      E.raise_TypeCheckError "ForExp dismatch" exploc.loc

  | A.Break loc ->
    if  inloop then set_type trf T.Nil
    else 
      E.raise_TypeCheckError "Break out of loop" exploc.loc

  | A.Let (declist,e) -> 
    let newenv = check_declist env declist in
    let expty = check_exp newenv e in
    set_type trf expty

and check_var var loc ({v=venv;t=tenv;inloop = inloop} as env) = match var with
    (*从变量环境中,查找与该实例绑定的类型*)
    | A.IdVar sb ->
    begin
      match S.get sb venv with 
      | Some (ty) -> ty
      | _ -> E.raise_TypeCheckError "not a var" loc
    end
    (*必须为数组类型,且index为整数*)
    | A.SubscriptVar (var,e) ->
      let varty = check_var var loc env in
      let expty =  check_exp env e in
      if check_Int expty 
      then
        match varty with
        | T.Array _ as ty -> ty
        | _ -> E.raise_TypeCheckError "not a array typr" loc
      else E.raise_TypeCheckError "not a subscrip is not int" loc

    (*必须为记录类型,且id在fieldlist中*)
    | A.FieldExp (var,sb) ->
      let varty = check_var var loc env in
      match varty with 
      | T.Record (sb_tylist,_) ->
        begin 
          try 
            List.assoc sb sb_tylist 
          with 
            Not_found -> E.raise_TypeCheckError "not a array typr" loc
        end
      | _ ->  E.raise_TypeCheckError "not a record typr" loc

and check_exp_loc_list env = function
  | [] -> T.Nil
  | [exp_loc] -> check_exp env (exp_loc)
  | hd::tl -> ignore (check_exp env hd); check_exp_loc_list env tl

and check_record sb_tylist sb_loclist env =
    let actualtylsit = sb_loclist |>
        List.map (fun (_,loc) -> check_exp env loc) in
    let expectedtylist = sb_tylist |>
        List.map (fun (_,ty) -> ty) in
        T.check_tylist  actualtylsit expectedtylist

(*对于一个declist,每一次dec所产生的env,提供给下一个dec*)
and check_declist env declist =
  declist |>
  List.fold_left (fun env dec->check_dec env dec) env 

and check_dec ({v=venv;t=tenv;inloop=inloop} as env) dec =
(*检查变量声明符合类型要求,并返回一个新环境*)
  match dec with
  | A.TyDec {type_name;type_type} ->
    (*根据类型symbol找到相符的真实类型,并将类型加入类型环境*)
    let ty =  tranFromAtytoTty tenv type_type in
    add_tenv type_name ty env 

  | A.VarDec {var_name;
              var_type;
              var_exp}  ->
    (*检查变量声明符合要求,并将变量加入变量字典*)
    let actualty = check_exp env var_exp in
    let (loc,_) = var_exp in
    let expectedty = tylookorfail  tenv var_name in
    if T.check_ty actualty expectedty 
    then 
      begin 
         add_venv var_name actualty env 
      end
    else E.raise_TypeCheckError "id dismatched tyid" loc.loc

  | A.FunDec {fun_name;
              fun_type;
              fun_fields;
              fun_exp} -> 

    (*假设的新环境*)
    let newenv =add_fieldlist_to_venv  env fun_fields in
    (*在新环境下函数的类型*)
    let actualty = check_exp newenv fun_exp in
    let expectedty = tylookorfail  tenv fun_name in
    let tylist= trantotylist env fun_fields in
    let (loc,_) = fun_exp in 
    if T.check_ty actualty expectedty 
    then 
      begin
        let ty = T.Function (tylist,actualty) in
        add_tenv fun_name ty env
      end
    else E.raise_TypeCheckError "FunDec dismatched " loc.loc

and tranFromAtytoTty tenv aty  = match aty with
  | A.Tyid sb -> tylookorfail tenv sb 
  | A.RecTy fieldlist -> tranFromfieldlist tenv fieldlist 
  | A.ArrTy (loc) -> 
    let ty = tylookorfail tenv loc in
    T.Array (ty,T.incridex ())

(*将id:tyid转化为记录类型*)
and tranFromfieldlist tenv fieldlist = 
  let sb_tylist =
  List.map (fun field ->
    let (id,tyid) = field in
    let ty = tylookorfail tenv tyid  in
    (id,ty)) fieldlist  in
  T.Record(sb_tylist,T.incridex ())

and trantotylist env fieldlist =
List.map (fun (_,sb) -> lookup_venv sb env) fieldlist

and add_fieldlist_to_venv ({v=venv;t=tenv;inloop=inloop} as env) fieldlist =
  List.fold_left 
  (fun e (sb1,sb2) -> 
    let ty = lookup_tenv sb2  env in
    add_venv sb1 ty e)
  env 
  fieldlist