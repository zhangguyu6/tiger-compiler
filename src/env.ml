module S = Symtab
module U = Unique

type ty = 
  | TInt
  | TFloat
  | TBool
  | TString
  | TRecord of (S.t * ty) list * U.t
  | TArray of ty * U.t
  | TNil
  | TName of S.t * ty option ref

let rec check_ty x y =
  match (x,y) with
  | (TInt,TInt) -> true
  | (TFloat,TFloat) -> true
  | (TBool,TBool) -> true
  | (TString,TString) -> true
  | (TNil,TNil) -> true
  | (TArray (ty1,_),TArray (ty2,_)) -> check_ty ty1 ty2
  | (TRecord (styl1,_) ,TRecord (styl2,_)) ->
    begin
      let tyl1 = List.map (fun (_,x) -> x) styl1 in 
      let tyl2 = List.map (fun (_,x) -> x) styl2 in 
      check_tylist tyl1 tyl2
    end
  | (TName(_,rty),other) | (other,TName(rty,_)) ->
    match !rty with
    | Some ty -> check_ty ty other
    | None -> false
  | _ -> false

let check_tylist tyl1 tyl2 =
  List.iter2 
  (fun x y -> if not (check_ty x y) then failwith "typelist dismatched")
  tyl1 tyl2 ; true




type entry =
  (*值类型*)
  | VarEntry of ty
  (*函数类型 (参数类型):返回类型*)
  | FunEntry of ty list * ty