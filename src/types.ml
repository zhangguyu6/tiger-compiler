(*tiger语言的基本类型*)
open Batteries
open Batteries.Printf
open Symtab

(*一个自增索引,用来判断record和array类型是否相似*)
type unique = int ref

(*通过引用类型实现了这个索引,每调用一次索引加一*)
let id=ref (-1)

let incridex () = 
   incr id ;id;;

let string_of_id (id:unique):string =
  sprintf "#id: %d #\n" !id;;

(*bigtiger语言的标准类型,5个基本类型,4个引用类型*)
type ty = 
  | Int
  | Float
  | Bool
  | String
  | Nil
  (*Record (id:ty+)*)
  | Record of (symbol * ty) list * unique
  (*Array [ty+]*)
  | Array of ty * unique
  (*id:ty*)
  | Name of symbol * ty option ref
  (*Function (ty +):ty*)
  | Function of ty list * ty

(*递归地检查类型是否相符*)
let rec check_ty (x:ty) (y:ty) :bool =
  match x,y with
  | Int,Int -> true
  | Float,Float -> true
  | Bool,Bool -> true
  | String,String -> true
  | Nil,Nil -> true
  | Array (_,u1),Array (_,u2) -> !u1 = !u2
  | Record (_,u1) ,Record (_,u2) -> !u1 = !u2
  | Name(_,rty),other | other,Name(_,rty) ->
  begin
    match !rty with
    | Some ty -> check_ty ty other
    | None -> false
  end
  | Function (args1,results1),Function (args2,results2) ->
    false
  | _-> false

(*检查两个类型列表是否相符,只有在长度相等且每个元素的都相符时才相符*)
let check_tylist (l1:ty list) (l2:ty list) : bool =
  if List.length l1 != List.length l2 then false
  else 
  List.fold_left2 (fun acc t1 t2 -> acc && check_ty t1 t2) true l1 l2 

(*打印类型*)
let rec string_of_ty (ty:ty) :string= match ty with
  | Int -> "Int"
  | Float -> "Float"
  | Bool -> "Bool"
  | String -> "String"
  | Nil -> "Nil"
  | Record (fields,_) -> sprintf "Record {%s} " (string_of_fields fields)
  | Array (ty,_) -> sprintf "Array [%s]" (string_of_ty ty)
  | Name (id,rty) ->
  begin
      match !rty with
    | Some ty -> sprintf "Name <%s>:%s" (to_string id) (string_of_ty ty)
    | None -> "Name"
  end
  | Function (tylist,ty) -> sprintf "Function (%s):%s" (string_of_tylist tylist) (string_of_ty ty)

and string_of_fields (fieldlist:(symbol * ty) list):string=
  List.fold_left 
  (fun acc (s,ty) -> sprintf "%s, %s:%s" acc (to_string s) (string_of_ty ty)) 
  "" 
  fieldlist

and string_of_tylist (tylist:ty list) : string =
  tylist |>
  List.fold_left (fun acc ty -> sprintf "%s,%s" acc (string_of_ty ty)) ""