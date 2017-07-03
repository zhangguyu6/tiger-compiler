%{
  open Ast
%}
/* all keywords of tiger 全部关键字*/
%token ARRAY BREAK DO ELSE END FOR FUNCTION IF IN LET NIL OF THEN TO TYPE VAR WHILE
/* tiger 的 全部标点 */
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
%token eof
/* 略微扩展了tiger的类型系统，增加了小数和布尔类型 */
%token <int> INTCONST
%token <float> FLOATCONST
%token <bool> BOOLCONST
%token <string> STRINGCONST
%token <string> ID

/*从上到下，优先级逐步升高*/
%right FUNCTION TYPE
%nonassoc WHILE FOR
%right ELSE DO
%nonassoc ASSIGNMENT
%left OR
%left AND
%nonassoc EQ NOTEQ  GT GE LT LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc unary_over_binary

%%