%{
  open Location
  module A = Ast2
  module T = Types
  module S = Symtab
  let mkexp raw_exp = (raw_exp,ref T.Nil)
  let mkloc exp startpos endpos = 
  {
    value=exp;
    loc={startpos=startpos;endpos=endpos}
    }
%}
/* all keywords of tiger 全部关键字*/
%token ARRAY BREAK DO ELSE END FOR IF IN LET NIL OF THEN TO WHILE
%token TYPE VAR FUNCTION
/* tiger 的 全部标点 , : ; . */
%token COMMA COLON SEMI DOT
/* ( ) { } [ ] )*/
%token LPAREN RPAREN LBRACE RBRACE LBracket RBracket
/* + - * / */
%token PLUS MINUS TIMES DIVIDE 
/* = <> > >= < <= */
%token EQ NOTEQ  GT GE LT LE
/* & | not := */
%token AND OR NOT ASSIGNMENT
/* 终结符*/
%token EOF
/* 略微扩展了tiger的类型系统，增加了小数和布尔类型 */
%token <int> INTCONST
%token <float> FLOATCONST
%token <bool> BOOLCONST
%token <string> STRINGCONST
%token <string> ID

/*从上到下，优先级逐步升高*/
%nonassoc ELSE DO
%nonassoc ASSIGNMENT
%left OR
%left AND
%nonassoc EQ NOTEQ  GT GE LT LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc unary_over_binary

%start <Ast2.exp> prog
%%

%inline unaryop:
| NOT {A.Not}

%inline binop:
| PLUS {A.Plus}
| TIMES {A.Times}
| DIVIDE {A.Divide}
| EQ  {A.Eq}
| NOTEQ {A.Neq}
| GT {A.Gt}
| GE {A.Ge}
| LT {A.Li}
| LE {A.Le}
| AND {A.And}
| OR {A.Or}



prog:
| e=exp EOF {e}

exp:

/*基本类型*/
| NIL {mkexp (A.NilExp ())}
| i=INTCONST {mkexp (A.IntEXp  i)}
| s=STRINGCONST {mkexp (A.StringEXp s)}
| b=BOOLCONST {mkexp (A.BoolExp b)}
| f=FLOATCONST {mkexp (A.FloatExp f)}
/*基本引用类型*/
| ty=id LBracket size=exp RBracket OF init=exp
{ let sb = S.from_string ty in
  let loc1 = mkloc size $startpos(size) $endpos(size) in
  let loc2 = mkloc init $startpos(init) $endpos(init) in
  mkexp (A.ArrayCreate (sb, loc1, loc2))
} 
| ty=id LBRACE fields=separated_list(COMMA,field) RBRACE
{ let sb = S.from_string ty in
  mkexp (A.RecordCreate (sb ,fields))}
/*基本组合类型*/
| u=unaryop e=exp  %prec unary_over_binary 
{let loc1 = mkloc u $startpos(u) $endpos(u) in
 let loc2 = mkloc e $startpos(e) $endpos(e) in
 mkexp (A.UnaryExp (loc1,loc2))}

| e1=exp b=binop e2=exp
{let loc0 = mkloc b $startpos(b) $endpos(b) in
 let loc1 = mkloc e1 $startpos(e1) $endpos(e1) in
 let loc2 = mkloc e2 $startpos(e2) $endpos(e2) in
 mkexp (A.InfixOpExp (loc0, loc1, loc2))}

| MINUS e=exp  %prec unary_over_binary 
{let loc1 = mkloc A.Neg $startpos($1) $endpos($1) in
 let loc2 = mkloc e $startpos(e) $endpos(e) in
 mkexp (A.UnaryExp (loc1,loc2))}

| e1=exp MINUS e2=exp
{let loc0 = mkloc A.Minus $startpos($2) $endpos($2) in
 let loc1 = mkloc e1 $startpos(e1) $endpos(e1) in
 let loc2 = mkloc e2 $startpos(e2) $endpos(e2) in
 mkexp (A.InfixOpExp (loc0, loc1, loc2))}

 /*变量*/
| v= lvalue 
  {let loc = mkloc  v $startpos $endpos in
   mkexp (A.LValueExp loc)}
 /*表达式序列*/
| LPAREN explist = sequence RPAREN {explist} 

 /*函数调用*/
| ty=id LPAREN args=separated_list(COMMA,exp) RPAREN
  {let sb =S.from_string ty in
   mkexp (A.CallExp (sb,args))}
/*赋值*/
| v=lvalue ASSIGNMENT e=exp
  {let loc1 = mkloc  v $startpos(v) $endpos(v) in
   let loc2 = mkloc  e $startpos(e) $endpos(e) in
   mkexp (A.Assign (loc1,loc2))
  }
/*ifthenelse*/
| IF ife=exp THEN thene=exp ELSE e=exp
  {let ifloc = mkloc ife  $startpos(ife) $endpos(ife) in
   let thenloc = mkloc thene $startpos(thene) $endpos(thene) in
   let elseloc = mkloc e $startpos(e) $endpos(e) in
   mkexp (A.IfThenElse (ifloc,thenloc,elseloc))
   }
  /*if then*/
| IF ife=exp THEN thene=exp
  {let ifloc = mkloc ife  $startpos(ife) $endpos(ife) in
   let thenloc = mkloc thene $startpos(thene) $endpos(thene) in
   mkexp (A.IfThen (ifloc,thenloc))
   }
/*while do*/
| WHILE whileexp=exp DO doexp=exp
  {let whileloc = mkloc whileexp  $startpos(whileexp) $endpos(whileexp) in
   let doloc = mkloc doexp  $startpos(doexp) $endpos(doexp) in
   mkexp (A.WhileExp (whileloc,doloc))
  }
/*for i := 0 to 10 do e*/
| FOR ty=id ASSIGNMENT e1=exp TO e2=exp DO e3=exp
  {let sb =  S.from_string ty in 
   let esp = ref true in
   let loc1 = mkloc e1  $startpos(e1) $endpos(e1) in
   let loc2 = mkloc e2  $startpos(e2) $endpos(e2) in
   let loc3 = mkloc e3  $startpos(e3) $endpos(e3) in
   mkexp (A.ForExp (sb,esp,loc1,loc2,loc3))
   }
/*break*/
| BREAK
  {mkexp (A.Break (mkloc () $startpos $endpos))}
/*let dec,dec,dec  in  exp end*/
| LET declist = separated_list(COMMA,dec) IN e=exp END
  {mkexp (A.Let (declist,mkloc e $startpos(e) $endpos(e)))}


lvalue:
| i=id 
 {A.IdVar (S.from_string i)}
| v=lvalue LBracket e=exp RBracket 
 {let loc1 = mkloc v $startpos(v) $endpos(v) in
  let loc2 = mkloc e $startpos(e) $endpos(e) in
    A.SubscriptVar (loc1,loc2)}
| v=lvalue DOT i=id
  {let loc =mkloc v $startpos(v) $endpos(v) in
   let sb = S.from_string i in
   A.FieldExp (loc,sb)}

field:
| ty=id EQ e=exp
  {let sb = S.from_string ty in
   let loc = mkloc e $startpos(e) $endpos(e) in
    (sb,loc)}

id :
| i= ID {i}

dec:
| d = tydec {d}
| d = vardec {d}
| d = fundec {d}

tydec:
| TYPE tyid=id t=ty
  {let ty_name = S.from_string tyid in
   A.TyDec {
     A.type_name =ty_name;
     A.type_type = t
   }}

ty:
| tyid = id
{A.Tyid (S.from_string tyid)}
| ARRAY OF tyid=id
{A.ArrTy (S.from_string tyid)}
| LBRACE fields = separated_list(COMMA,fielddec) RBRACE
{A.RecTy fields}

vardec:
| VAR name=id COLON ty=id ASSIGNMENT e=exp
  { A.VarDec {
    A.var_name = S.from_string name;
    A.var_type = S.from_string ty;
    A.var_exp = mkloc e $startpos(e) $endpos(e);
    A.var_escape = ref true}}

fundec:
| FUNCTION name=id LBRACE fields = separated_list(COMMA,fielddec) RBRACE COLON ty=id EQ e=exp
{let fun_name = S.from_string name in
 let fun_type = S.from_string ty in
 let fun_exp = mkloc e $startpos(e) $endpos(e) in
  A.FunDec {A.fun_name = fun_name;
  A.fun_type = fun_type;
  A.fun_fields = fields;
  A.fun_exp = fun_exp} }

fielddec:
| name=id COLON ty=id
  {let sb1 = S.from_string name in
   let sb2 = S.from_string ty in
   (sb1,ref true,sb2)}

sequence:
| elist=separated_list(SEMI,exp) {mkexp (A.SeqExp elist)}

