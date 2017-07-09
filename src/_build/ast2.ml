(*parser生成的抽象语法树*)

open Batteries
open BatPrintf
open Location
module S = Symtab
module T = Types

type infixop =
  (*中缀操作符*)
  | Plus | Minus | Times | Divide | Eq | Neq | Li | Le | Gt | Ge | And | Or

type unaryop =
  (*前缀操作符*)
  | Neg | Not

type ty =
  (*tyid arrty recty 三种类型*)
  | Tyid of S.symbol
  (*record类型引用的变量要进行逃逸分析*)
  | RecTy of field list
  (*array[tyid]*)
  | ArrTy of S.symbol

and field =
  (*record和fundec的参数变量 id:tyid *)
  S.symbol * bool ref * S.symbol

(*exp=value+type*)
type exp = raw_exp * T.ty ref

and raw_exp =

  (*基本类型*)
  | NilExp of unit
  | IntExp of int
  | StringEXp of string
  | BoolExp of bool 
  | FloatExp of float

  (*复合类型需要记录位置信息*)
  (*变量 id subsript fieldExp*)
  | LValueExp of var loc 
  (*表达式序列*)
  | SeqExp of exp list
  (*中缀操作符*)
  | InfixOpExp of infixop loc * 
                  (*左表达式*)
                  exp loc *  
                  (*右表达式*)   
                  exp loc      
  (*前缀操作符*)
  | UnaryExp of unaryop loc *
                (*表达式*)
                exp loc      
  (*函数调用*)
  | CallExp of S.symbol * (*函数名*)
               exp list (*参数列表*)
  (*列表*)
  | ArrayCreate of S.symbol * (*类型*)
                   exp loc * (*数组大小*)
                   exp loc   (*数组初始化*)
  (*record*)
  | RecordCreate of S.symbol *(*类型*)
                    (S.symbol * exp loc) list (*fields*)
  (*赋值 id:=exp*)
  | Assign of var loc * (*变量名*)
              exp loc   (*变量值*)
  
  | IfThenElse of exp loc * (*if*)
                  exp loc * (*then*)
                  exp loc   (*else*)

  | IfThen of exp loc * (*if*)
              exp loc  (*then*)

  | WhileExp of exp loc * (*while*)
                exp loc   (*do*)

  | ForExp of S.symbol *        (*循环变量*)
              bool ref *   (*逃逸参数，判断是否放入栈帧*)
              exp loc * (*初始值*)
              exp loc * (*终结值*)
              exp loc   (*do*)

  | Break of unit loc
  (*let var in exp*)
  | Let of dec list * (*声明的变量*)
           exp loc 

and dec = 
  | TyDec of tydec
  | VarDec of vardec
  | FunDec of fundec

and var = 
  (*id var[exp] var.id *)
  | IdVar of S.symbol
  | SubscriptVar of var loc * exp loc
  | FieldExp of var loc * S.symbol

and tydec = 
(*type tyid = ty*)
{
  type_name : S.symbol;
  type_type: ty
}

and vardec = 
(*var id : tyid :=exp*)
{
  var_name: S.symbol;
  var_type: S.symbol;
  var_exp : exp loc;
  var_escape:bool ref  (*逃逸参数，判断是否放入栈帧*)
}

and fundec = 
(*function id (field lsit) :tyid = exp*)
{
  fun_name: S.symbol;
  fun_type: S.symbol;
  fun_fields: field list;
  fun_exp : exp loc
}

let rec string_of_exp ((e,refty):exp):string  = match e with
  (*基本表达式*)
  | NilExp _-> "nil" 
  | IntExp i -> string_of_int i
  | StringEXp s-> s
  | BoolExp b -> string_of_bool b
  | FloatExp f -> string_of_float f
  (*复合表达式*)
  | LValueExp loc -> string_of_var loc.value

  | SeqExp  explist -> string_of_explist explist

  | InfixOpExp (loc0,loc1,loc2) -> 
    let op = loc0.value in 
    let exp1 = loc1.value in
    let exp2 = loc2.value in 
    sprintf "%s %s %s" (string_of_binop op) (string_of_exp exp1) (string_of_exp exp2)

  | UnaryExp (loc0,loc1) ->
    let op = loc0.value in 
    let exp1 = loc1.value in
    sprintf "%s %s" (string_of_unop op) (string_of_exp exp1)

  | CallExp (sb,loc) ->
    let name = S.to_string sb in
    let args = string_of_explist loc in 
    sprintf "callfun:%s(%s)" name args

  | ArrayCreate (sb,loc1,loc2) ->
  begin 
    let ty = S.to_string sb in 
    let exp1 = loc1.value in 
    let exp2 = loc2.value in
    sprintf "Array tyId:%s size:%s of:%s" ty (string_of_exp exp1) (string_of_exp exp2)
  end

  | RecordCreate (sb,fcreatelist) -> 
  begin
    let ty= S.to_string sb in
    let fields = fcreatelist |> string_of_fcreatelist in
    sprintf "RecordCreate tyId:%s fieldCreate:%s" ty fields
  end

  | Assign (loc1,loc2) ->
    let idname = string_of_var loc1.value in
    let exp = loc2.value in 
    sprintf "%s:=%s" idname (string_of_exp exp)

  | IfThenElse (loc0,loc1,loc2) ->
    let ifexp = string_of_exp loc0.value in 
    let thenexp = string_of_exp loc1.value in 
    let elseexp = string_of_exp loc2.value in
    sprintf "if (%s) then (%s) else(%s)" ifexp thenexp elseexp

  | IfThen (loc0,loc1) ->
    let ifexp = string_of_exp loc0.value in 
    let thenexp = string_of_exp loc1.value in 
    sprintf "if (%s) then (%s)" ifexp thenexp

  | WhileExp (loc0,loc1) ->
    let whileexp = string_of_exp loc0.value in 
    let doexp = string_of_exp loc1.value in 
    sprintf "while (%s) do {%s}" whileexp doexp

  | ForExp (sb,_,loc0,loc1,loc2) ->
    let varid =S.to_string sb in
    let initexp =  string_of_exp loc0.value in
    let endexp =  string_of_exp loc1.value in
    let doexp = string_of_exp loc1.value in
    sprintf "for %s:=%s to %s do {%s}" varid initexp endexp doexp

  | Break _-> "break"

  | Let (declist,exp) -> 
    sprintf "let %s in %s end" (string_of_declist declist) (string_of_exp exp.value)


and string_of_var (v:var):string =match v with
    | IdVar sb -> S.to_string sb
    | SubscriptVar (idvar,valexp) -> 
      let var = idvar.value in
      let valexp = string_of_exp valexp.value in
      sprintf "%s [%s]" (string_of_var var) valexp
    | FieldExp(idvar,sb) -> 
      let var = string_of_var idvar.value in
      let id = S.to_string sb in
    sprintf "%s.%s" var id

and string_of_binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Eq -> "="
    | Li -> "<"
    | Le -> "<="
    | Gt ->">"
    | Ge ->">="
    | And -> "&"
    | Or -> "|"
    | Neq -> "<>"

and string_of_unop = function
    | Neg ->"-"
    | Not ->"not"

and string_of_ty = function
    | Tyid sb -> "tyId:"^ (S.to_string sb)
    | RecTy fieldlist -> "Record:"^(string_of_fieldlist fieldlist)
    | ArrTy loc -> "Array:"^(S.to_string loc)

and string_of_fieldlist (fieldlist:field list):string=
    fieldlist |>
    List.map (fun x -> 
    let sb1,_,sb2 = x in (S.to_string sb1) ^ ":" ^ (S.to_string sb2)) |> 
    List.fold_left (fun acc x -> acc ^ x) ""
    

and string_of_fcreatelist fcreatelist = 
    fcreatelist |>
    List.map (fun (sb,fieldexp) ->  
    let id = S.to_string sb in
    let exp =fieldexp.value|> string_of_exp  in
    sprintf "%s:%s" id exp) |>
    List.fold_left (fun acc x -> acc^";"^ x) ""

and string_of_explist explist= 
    explist |>
    List.fold_left (fun acc x -> acc^";"^(x |>  string_of_exp)) ""

and string_of_dec = function
  | TyDec  dec -> string_of_tydec dec
  | VarDec dec -> string_of_vardec dec
  | FunDec dec -> string_of_fundec dec

and string_of_tydec {type_name;type_type} = 
    let tyname= type_name |>  S.to_string in 
    let ty = type_type |> string_of_ty in
    sprintf "type %s=%s" tyname ty

and string_of_vardec {var_name;var_type;var_exp;var_escape} = 
    let id = var_name |>  S.to_string in 
    let ty= var_type |>  S.to_string in 
    let varexp = var_exp.value|> string_of_exp in
    sprintf "var %s : %s := %s" id ty varexp

and string_of_fundec { fun_name;fun_type;fun_fields;fun_exp} =
    let id = fun_name |> S.to_string in
    let ty = fun_type |> S.to_string in
    let fieldlist = fun_fields |> string_of_fieldlist in
    let funexp = fun_exp.value |> string_of_exp in
    sprintf "function %s (%s):(%s) (%s)" id ty fieldlist funexp

and string_of_declist declist= 
  declist |>
  List.map (fun x -> string_of_dec x) |>
  List.fold_left (fun acc x -> acc^";"^x) ""