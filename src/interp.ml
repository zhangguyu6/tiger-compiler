open Batteries
open BatPrintf
open Location
open Symtab
open Env
open Errors
module A = Ast

let rec eval ((valenv,inloop) as env) ((exploc ,trf) as exp) = 
  match exploc.value with
    (*基本类型*)
    | A.NilExp _ -> VNil ()
    | A.IntExp i -> VInt i
    | A.StringEXp s -> VString s
    | A.BoolExp b -> VBool b
    | A.FloatExp f -> VFloat f

    | A.UnaryExp (op,e) -> 
      begin 
        match eval env e with
        | VInt i -> VInt (-i)
        | VFloat f -> VFloat (-.f)
        | VBool b -> VBool (not b)
        | _ -> raise_RuntimeError "UnaryExp dismatch" exploc.loc
      end
    
    | InfixOpExp (op,e1,e2) ->
      begin
        match op with
        | A.Plus -> 
        begin 
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VInt(i1+i2) 
          | VFloat f1,VFloat f2 -> VFloat (f1+.f2)
          | _-> raise_RuntimeError "Plus dismatch" exploc.loc
        end
        | A.Minus ->
        begin 
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VInt(i1-i2) 
          | VFloat f1,VFloat f2 -> VFloat (f1-.f2)
          | _-> raise_RuntimeError "Minus dismatch" exploc.loc
        end
        | A.Times ->
        begin 
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VInt(i1*i2) 
          | VFloat f1,VFloat f2 -> VFloat (f1*.f2)
          | _-> raise_RuntimeError "Times dismatch" exploc.loc
        end
        | A.Divide ->
        begin 
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VInt(i1/i2) 
          | VFloat f1,VFloat f2 -> VFloat (f1/.f2)
          | _-> raise_RuntimeError "Divide dismatch" exploc.loc
        end
        | A.Eq ->
        begin 
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VBool(i1=i2) 
          | VFloat f1,VFloat f2 -> VBool(f1=f2)
          | VBool b1,VBool b2 -> VBool(b1=b2)
          | VString b1 , VString b2 -> VBool(b1=b2)
          | _-> raise_RuntimeError "Eq dismatch" exploc.loc
        end
        | A.Neq ->
        begin 
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VBool(i1 != i2) 
          | VFloat f1,VFloat f2 -> VBool(f1 != f2)
          | VBool b1,VBool b2 -> VBool(b1 != b2)
          | VString b1 , VString b2 -> VBool(b1 != b2)
          | _-> raise_RuntimeError "Neq dismatch" exploc.loc
        end
        | A.Li ->
        begin
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VBool(i1 < i2) 
          | VFloat f1,VFloat f2 -> VBool(f1 < f2)
          | _-> raise_RuntimeError "Li dismatch" exploc.loc
        end
        | A.Le ->
        begin
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VBool(i1 <= i2) 
          | VFloat f1,VFloat f2 -> VBool(f1 <= f2)
          | _-> raise_RuntimeError "Le dismatch" exploc.loc
        end
        | A.Gt ->
        begin
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VBool(i1 > i2) 
          | VFloat f1,VFloat f2 -> VBool(f1 > f2)
          | _-> raise_RuntimeError "Gt dismatch" exploc.loc
        end
        | A.Ge ->
        begin
          match eval env e1, eval env e2 with
          | VInt i1 , VInt i2 -> VBool(i1 >= i2) 
          | VFloat f1,VFloat f2 -> VBool(f1 >= f2)
          | _-> raise_RuntimeError "Ge dismatch" exploc.loc
        end
        | A.And ->
        begin
          match eval env e1, eval env e2 with
          | VBool b1 , VBool b2 -> VBool(b1&&b2) 
          | _-> raise_RuntimeError "And dismatch" exploc.loc
        end                              
        | A.Or ->
        begin
          match eval env e1, eval env e2 with
          | VBool b1 , VBool b2 -> VBool(b1||b2) 
          | _-> raise_RuntimeError "Or dismatch" exploc.loc
        end
      end   
    (*基本数据结构*)
    | A.ArrayCreate (sb,e1,e2) ->
      let VInt size = eval env e1 in
      let initval = eval env e2 in
      VArray (Array.make size initval)
    | A.RecordCreate (sb,sb_explist) ->
      let fields=
      List.map (fun (sb,exp) -> (sb,eval env exp)) sb_explist in
      VRecord fields

    | A.IfThenElse(e1,e2,e3) ->
      begin
        match  eval env e1 with
        | VBool b when b=true  -> eval env e2
        | VBool b when b=false -> eval env e3
        | _ -> raise_RuntimeError "IfThenElse dismatch" exploc.loc
      end

    | A.IfThen(e1,e2) ->
      begin
        match  eval env e1 with
        | VBool b when b=true  -> eval env e2
        | VBool b when b=false -> VNil ()
        | _ -> raise_RuntimeError "IfThenElse dismatch" exploc.loc
      end
    
    | A.Break _ -> VNil ()
    
    | A.WhileExp (e1, e2) -> 
      let rec loop ((valenv,inloop) as env) =
          match eval env e1,inloop with
          | _,true -> VNil ()
          | VBool b,false when b -> eval env e2;loop env
          | VBool b,false when not b -> VNil ()
      in loop env

    | A.ForExp (sb,_,e1,e2,e3) ->
    let initval = eval env e1 in
    let endval = eval env e2 in
    let newenv = create_sb sb valenv initval in
    let rec loop ((valenv,inloop) as env) =
      if not inloop then VNil ()
      else
        match get_by_sb sb valenv, endval with
        | VInt i1 ,VInt i2 when i1 != i2 -> ignore(eval env e3);loop env
        | VInt i1,VInt i2 when i1 = i2 -> VNil ()
        | _ -> raise_RuntimeError "ForExp dismatch" exploc.loc
    in loop (newenv,inloop)

    | A.Assign (var,e) ->
    begin
      let expval = eval env e in
      match var with
      | A.IdVar s -> 
      bind_sb s valenv expval;VNil () 

      | A.SubscriptVar (A.IdVar s,e) ->
      let VArray a = get_by_sb s valenv in
      let VInt index = expval in
      if index <0 || index > Array.length a -1 
      then raise_RuntimeError "index overload" exploc.loc
      else Array.set a index expval;VNil ()

      | A.FieldExp (A.IdVar s,sb) ->
      let VRecord sy_val_list = get_by_sb s valenv in
      if List.mem_assoc sb sy_val_list
      then VRecord (List.modify sb (fun x -> expval) sy_val_list)
      else raise_RuntimeError "can't find field" exploc.loc
    end

    | A.Let(declist,e) ->
      let newenv = eval_declist env declist in
      eval newenv e

and eval_declist env declist= 
List.fold_left (fun e dec -> eval_dec e dec) env declist

and eval_dec env dec = 
  match dec with
  | A.TyDec {type_name;type_type} -> env

  | A.VarDec {var_name;
              var_type;
              var_exp} ->
    let (valenv,inloop) = env in
    let v = eval env var_exp in
    (create_sb var_name valenv v,inloop)

  | A.FunDec {fun_name;
              fun_type;
              fun_fields;
              fun_exp} ->
    let (valenv,inloop) = env in
    let sblist = List.map (fun (sb,_) -> sb) fun_fields in
    let v = VFunction (sblist,fun_exp) in
    (create_sb fun_name valenv v,inloop)







    
