open Batteries
open BatPrintf
module L = Location
module S = Symtab


type infixop =
  (*中缀操作符*)
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Li
  | Le
  | Gt
  | Ge
  | And
  | Or

type unaryop =
  (*前缀操作符*)
  | Neg
  | Not

type ty =
  (*tyid arrty recty 三种类型*)
  | Tyid of S.t L.info
  (*record类型引用的变量要进行逃逸分析*)
  | RecTy of field list
  | ArrTy of S.t L.info

type field =
  (*record和fundec的参数变量 id:tyid*)
  {
    field_name : S.t L.info;
    field_escape : bool ref;    (*逃逸参数，判断是否放入栈帧*)
    field_type : S.t L.info
  } 

type dec = 
  | TyDec of tydec 
  | VarDec of vardec
  | FunDec of fundec

and exp =
  
  | LValueExp of var L.info (*变量的值*) (*id subsript fieldExp*)
  | NilExp of unit L.info
  | IntEXp of int L.info
  | StringEXp of string L.info
  | BoolExp of bool L.info
  | FloatExp of float L.info
  | SeqExp of exp L.info list
  | InfixOpExp of infixop L.info * (*中缀操作符*)
                  exp L.info *     (*左表达式*)
                  exp L.info       (*右表达式*)

  | UnaryExp of unaryop L.info * (*前缀操作符*)
                exp L.info      (*表达式*)

  | CallExp of S.t L.info * (*函数名*)
               exp L.info list (*参数列表*)

  | ArrayCreate of S.t L.info * (*类型*)
                   exp L.info * (*数组大小*)
                   exp L.info   (*数组初始化*)

  | RecordCreate of S.t L.info *(*类型*)
                    (S.t L.info * exp L.info) list (*fields*)

  | Assign of var L.info * (*变量名*)
              exp L.info   (*变量值*)

  | IfThenElse of exp L.info * (*if*)
                  exp L.info * (*then*)
                  exp L.info   (*else*)
  | IfThen of exp L.info * (*if*)
              exp L.info  (*then*)

  | WhileExp of exp L.info * (*while*)
                exp L.info   (*do*)

  | ForExp of S.t L.info*        (*循环变量*)
              bool ref *   (*逃逸参数，判断是否放入栈帧*)
              exp L.lnfo * (*初始值*)
              exp L.info * (*终结值*)
              exp L.info   (*do*)

  | Break of unit L.info

  | Let of dec list * (*声明的变量*)
           exp L.info 

and var = 
  (*id var[exp] var.id *)
  | IdVar of S.t L.info
  | SubscriptVar of var L.info * exp L.info
  | FieldExp of var L.info * S.t L.info

and tydec = 
(*type tyid = ty*)
{
  type_name : S.t L.info;
  type_type: ty
}

and vardec = 
(*var id : tyid :=exp*)
{
  var_name: S.t L.info;
  var_type: S.t L.info;
  var_exp : exp L.info;
  var_escape:bool ref  (*逃逸参数，判断是否放入栈帧*)
}

and fundec = 
(*function id (field lsit) :tyid = exp*)
{
  fun_name: S.t L.info;
  fun_type: S.t L.info;
  fun_fields: field list;
  fun_exp : exp L.info
}

let rec exp_to_string  = function
  | NilExp _-> "nil" 

  | IntEXp inf -> inf |> L.extract_value |> string_of_int 

  | StringEXp inf-> L.extract_value inf

  | BoolExp inf -> inf |> L.extract_value |> string_of_bool

  | FloatExp inf -> inf |> L.extract_value |> string_of_float

  | SeqExp  explist -> string_of_explist explist

  | LValueExp inf -> inf |> L.extract_value |> string_of_val

  | InfixOpExp (inf1,inf2,inf3) -> 
  begin 
    let ifop = L.extract_value inf1 in 
    let exp1 = L.extract_value inf2 in 
    let exp2 = L.extract_value inf3 in
    sprintf "%s %s %s" (string_of_op ifop) (exp_to_string exp1) (exp_to_string exp2)
  end

  | UnaryExp (inf1,inf2) ->
  begin 
    let unop = L.extract_value inf1 in 
    let exp1 = L.extract_value inf2 in 
    sprintf "%s %s" (string_of_op ifop) (exp_to_string exp1)
  end

  | CallExp (inf1,inf2) ->
  begin 
    let callfunname = inf1 |> L.extract_value |> S.to_string in 
    sprintf "callfun:%s(%s)" callfunname (string_of_explist inf2)
  end

  | ArrayCreate (inf1,inf2,inf3) ->
  begin 
    let ty = L.extract_value inf1 in 
    let exp1 = L.extract_value inf2 in 
    let exp2 = L.extract_value inf3 in
    sprintf "Array tyId:%s size:%s of:%s" (string_of_ty ty) (exp_to_string exp1) (exp_to_string exp2)
  end

  | RecordCreate (tyinf,fcreatelist) -> 
  begin
    let typename= tyinf |> L.extract_value |> S.to_string in
    let fields = fcreatelist |> string_of_fcreatelist in
    sprintf "RecordCreate tyId:%s fieldCreate:%s" typename fields
  end

  | Assign (id,inf) ->
    sprintf "%s:=%s" (id |> L.extract_value) (inf|>L.extract_value|>exp_to_string )

  | IfThenElse (inf1,inf2,inf3) ->
    let ifexp = L.extract_value inf1 in 
    let thenexp = L.extract_value inf2 in 
    let elseexp = L.extract_value inf3 in
    sprintf "if (%s) then (%s) else(%s)" (exp_to_string ifexp) (exp_to_string thenexp) (exp_to_string elseexp)

  | IfThen (inf1,inf2) ->
    let ifexp = L.extract_value inf1 in 
    let thenexp = L.extract_value inf2 in 
    sprintf "if (%s) then (%s)" (exp_to_string ifexp) (exp_to_string thenexp)

  | WhileExp (inf1,inf2) ->
    let whileexp = L.extract_value inf1 in 
    let doexp = L.extract_value inf2 in 
    sprintf "while (%s) do {%s}" (exp_to_string whileexp) (exp_to_string doexp)

  | ForExp (inf1,_,inf2,inf3,inf4) ->
    let varid = L.extract_value inf1 in
    let initexp =  L.extract_value inf2 in
    let endexp =  L.extract_value inf3 in
    let doexp = L.extract_value inf4 in
    sprintf "for %s:=%s to %s do {%s}" varid (exp_to_string initexp) (exp_to_string endexp) (exp_to_string doexp)

  | Break _-> "break"

  | Let (declist,exp) -> 
    sprintf "let %s in %s end" (string_of_declist declist) (exp_to_string exp)

  | _  -> failwith "Wrong Syntax" 

and string_of_val =function
    | IdVar inf-> inf |> L.extract_value |> S.to_string
    | SubscriptVar(idvar,valexp) -> sprintf "%s [%s]" (string_of_val idvar) (exp_to_string valexp)
    | FieldExp(idvar,valexp) -> sprintf "%s.%s" (string_of_val idvar) (exp_to_string valexp)


and string_of_op = function
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
    | Neg ->"-"
    | Not ->"not"

and string_of_ty = function
    | Tyid inf -> "tyId"^(inf |> L.extract_value |> S.to_string)
    | RecTy fieldlist -> "Record:"^(string_of_fieldlist fieldlist)
    | ArrTy inf -> "Array:"^(inf |> L.extract_value |> S.to_string)

and string_of_fieldlist fieldlist=
    fieldlist |>
    List.map (fun {field_name;field_escape;field_type} -> 
    (field_name |> L.extract_value |> S.to_string)^":"^
     field_type |> L.extract_value |> S.to_string)) |>
    List.fold_left (fun acc x -> acc^","^(exp_to_string x)) ""

and string_of_fcreatelist fcreatelist = 
    fcreatelist |>
    List.map (fun (id,fieldexp) ->  
    sprintf "%s:%s" (id|> L.extract_value |> S.to_string) (exp_to_string fieldexp)) |>
    List.fold_left (fun acc x -> acc^";"^(exp_to_string x)) ""

and string_of_explist explist= 
    explist |>
    List.map (fun info -> L.extract_value info) |>
    List.fold_left (fun acc x -> acc^";"^(exp_to_string x)) ""

and string_of_dec = function
  | TyDec  dec -> string_of_tydec dec
  | VarDec dec -> string_of_vardec dec
  | FunDec dec -> string_of_fundec dec

and string_of_tydec {type_name;type_type} = 
    let tyname= type_name |>  L.extract_value |> S.to_string in 
    let ty = type_type |> string_of_ty in
    sprintf "type %s=%s" tyname ty

and string_of_vardec {var_name;var_type;var_exp;var_escape} = 
    let id = var_name |>  L.extract_value |> S.to_string in 
    let ty= var_type |>  L.extract_value |> S.to_string in 
    let varexp = var_exp |>  L.extract_value |> exp_to_string in
    sprintf "var %s : %s := %s" id ty varexp

and string_of_fundec { fun_name;fun_type;fun_fields;fun_exp} =
    let id = fun_name |>  L.extract_value |> S.to_string in
    let ty = fun_type |>  L.extract_value |> S.to_string in
    let fieldlist = fun_fields |> string_of_fieldlist in
    let funexp = fun_exp |>  L.extract_value |> exp_to_string in
    sprintf "function %s (%s):ty (%s)" id ty fieldlist funexp

and string_of_ty = function
  | Tyid id -> id |>  L.extract_value |> S.to_string
  | RecTy fieldlist -> string_of_fieldlist fieldlist
  | ArrTy arrty -> arrty |>  L.extract_value |> S.to_string

and string_of_declist declist= 
  declist |>
  List.map (fun x -> string_of_dec x) |>
  List.fold_left (fun acc x -> acc^";"^(exp_to_string x)) ""