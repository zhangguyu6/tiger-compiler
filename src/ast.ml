(*parser生成的抽象语法树*)

open Batteries
open BatPrintf
open Location
open Symtab
module T = Types

type infixop =
  (*中缀操作符*)
  | Plus | Minus | Times | Divide | Eq | Neq | Li | Le | Gt | Ge | And | Or

type unaryop =
  (*前缀操作符*)
  | Neg | Not

type ty =
  (*在语法分析阶段,每个symbol视做一个类型,不对类型进行推导*)
  (*tyid arrty recty 三种类型*)
  | Tyid of symbol
  (*record类型引用的变量要进行逃逸分析*)
  | RecTy of field list
  (*array[tyid]*)
  | ArrTy of symbol

and field =
  (*record和fundec的参数变量 id:tyid *)
  symbol * symbol




(*exp=record{exp;loc} + ty*)
type exp = raw_exp loc * T.ty ref 

and raw_exp =

  (*基本类型*)
  | NilExp of unit
  | IntExp of int
  | StringEXp of string
  | BoolExp of bool 
  | FloatExp of float

  (*复合类型需要记录位置信息*)
  (*变量 id subsript fieldExp*)
  | LValueExp of var  
  (*表达式序列*)
  | SeqExp of exp list
  (*中缀操作符*)
  | InfixOpExp of infixop  * 
                  (*左表达式*)
                  exp  *  
                  (*右表达式*)   
                  exp       
  (*前缀操作符*)
  | UnaryExp of unaryop  *
                (*表达式*)
                exp       
  (*函数调用*)
  | CallExp of symbol * (*函数名*)
               exp list (*参数列表*)
  (*列表*)
  | ArrayCreate of symbol * (*类型标签*)
                   exp  * (*数组大小*)
                   exp    (*数组初始化*)
  (*record*)
  | RecordCreate of symbol *(*类型标签*)
                    (symbol * exp ) list (*fields*)
  (*赋值 id:=exp*)
  | Assign of var  * (*变量名*)
              exp    (*变量值*)
  
  | IfThenElse of exp  * (*if*)
                  exp  * (*then*)
                  exp    (*else*)

  | IfThen of exp  * (*if*)
              exp   (*then*)

  | WhileExp of exp  * (*while*)
                exp    (*do*)

  | ForExp of symbol *        (*循环变量*)
              bool ref *   (*逃逸参数，判断是否放入栈帧*)
              exp  * (*初始值*)
              exp  * (*终结值*)
              exp    (*do*)

  | Break of unit 
  (*let var in exp*)
  | Let of dec list * (*声明的变量*)
           exp  

and dec = 
  | TyDec of tydec
  | VarDec of vardec
  | FunDec of fundec

and var = 
  (*id var[exp] var.id *)
  | IdVar of symbol
  | SubscriptVar of var  * exp 
  | FieldExp of var  * symbol

and tydec = 
(*type tyid = ty*)
{
  type_name : symbol;
  type_type: ty
}

and vardec = 
(*var id : tyid :=exp*)
{
  var_name: symbol;
  var_type: symbol;
  var_exp : exp ;
}

and fundec = 
(*function id (field lsit) :tyid = exp*)
{
  fun_name: symbol;
  fun_type: symbol;
  fun_fields: field list;
  fun_exp : exp 
}

let ext ((e,refty):exp) =
  e.value

let rec string_of_exp ((e,refty):exp):string  = match e.value with
  (*基本表达式*)
  | NilExp _-> "nil" 
  | IntExp i -> string_of_int i
  | StringEXp s-> s
  | BoolExp b -> string_of_bool b
  | FloatExp f -> string_of_float f
  (*复合表达式*)
  | LValueExp v -> string_of_var v

  | SeqExp  explist -> string_of_explist explist

  | InfixOpExp (op,e1,e2) -> 
    sprintf "%s %s %s" (string_of_binop op) (string_of_exp e1) (string_of_exp e2)

  | UnaryExp (op,e) ->
    sprintf "%s %s" (string_of_unop op) (string_of_exp e)

  | CallExp (sb,args) ->
    let name = to_string sb in
    let args = string_of_explist args in 
    sprintf "callfun:%s(%s)" name args

  | ArrayCreate (sb,e1,e2) ->
  begin 
    let ty = to_string sb in 
    sprintf "Array tyId:%s size:%s of:%s" ty (string_of_exp e1) (string_of_exp e2)
  end

  | RecordCreate (sb,fcreatelist) -> 
  begin
    let ty= to_string sb in
    let fields = fcreatelist |> string_of_fcreatelist in
    sprintf "RecordCreate tyId:%s fieldCreate:%s" ty fields
  end

  | Assign (v,e) ->
    let idname = string_of_var v in
    sprintf "%s:=%s" idname (string_of_exp e)

  | IfThenElse (e1,e2,e3) ->
    sprintf "if (%s) then (%s) else(%s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)

  | IfThen (e1,e2) ->
    sprintf "if (%s) then (%s)" (string_of_exp e1) (string_of_exp e2)

  | WhileExp (e1,e2) ->
    let whileexp = string_of_exp e1 in 
    let doexp = string_of_exp e2 in 
    sprintf "while (%s) do {%s}" whileexp doexp

  | ForExp (sb,_,e1,e2,e3) ->
    let varid =to_string sb in
    let initexp =  string_of_exp e1 in
    let endexp =  string_of_exp e2 in
    let doexp = string_of_exp e3 in
    sprintf "for %s:=%s to %s do {%s}" varid initexp endexp doexp

  | Break _-> "break"

  | Let (declist,exp) -> 
    sprintf "let %s in %s end" (string_of_declist declist) (string_of_exp exp)


and string_of_var (v:var):string =match v with
    | IdVar sb -> to_string sb
    | SubscriptVar (var,valexp) -> 
      let valexp = string_of_exp valexp in
      sprintf "%s [%s]" (string_of_var var) valexp
    | FieldExp(idvar,sb) -> 
      let var = string_of_var idvar in
      let id = to_string sb in
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
    | Tyid sb -> "tyId:"^ (to_string sb)
    | RecTy fieldlist -> "Record:"^(string_of_fieldlist fieldlist)
    | ArrTy sb -> "Array:"^(to_string sb)

and string_of_fieldlist (fieldlist:field list):string=
    fieldlist |>
    List.map (fun x -> 
    let sb1,sb2 = x in (to_string sb1) ^ ":" ^ (to_string sb2)) |> 
    List.fold_left (fun acc x -> acc ^ x) ""
    

and string_of_fcreatelist fcreatelist = 
    fcreatelist |>
    List.map (fun (sb,fieldexp) ->  
    let id = to_string sb in
    let exp =fieldexp|> string_of_exp  in
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
    let tyname= type_name |>  to_string in 
    let ty = type_type |> string_of_ty in
    sprintf "type %s=%s" tyname ty

and string_of_vardec {var_name;var_type;var_exp;} = 
    let id = var_name |>  to_string in 
    let ty= var_type |>  to_string in 
    let varexp = var_exp|> string_of_exp in
    sprintf "var %s : %s := %s" id ty varexp

and string_of_fundec { fun_name;fun_type;fun_fields;fun_exp} =
    let id = fun_name |> to_string in
    let ty = fun_type |> to_string in
    let fieldlist = fun_fields |> string_of_fieldlist in
    let funexp = fun_exp |> string_of_exp in
    sprintf "function %s (%s):(%s) (%s)" id ty fieldlist funexp

and string_of_declist declist= 
  declist |>
  List.map (fun x -> string_of_dec x) |>
  List.fold_left (fun acc x -> acc^";"^x) ""