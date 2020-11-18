{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }
%token	

if			{ TOK_IF }
else        { TOK_ELSE }
while       { TOK_WHILE }
int 		{ TOK_INT }
main        { TOK_MAIN }
bool        { TOK_BOOL }
true 		{ TOK_TRUE }
false		{ TOK_FALSE }
return		{ TOK_RETURN }
print_int	{ TOK_PRINT_INT }
scan_int    { TOK_SCAN_INT }
num      	{ TOK_NUM $$ }
string      { ID $$ }
'+' 		{ TOK_PLUS }
'*'			{ TOK_TIMES }
'-'			{ TOK_MINUS }
'/'			{ TOK_DIV }
'%'			{ TOK_MOD }
'(' 		{ TOK_LPAREN }
')' 		{ TOK_RPAREN }
'}' 		{ TOK_RBRACE }
'{' 		{ TOK_LBRACE }
';' 		{ TOK_SEMICOLON }
','         { TOK_COMMA }
'=='		{ TOK_EQUAL }
'!='		{ TOK_DIFFERENT }
'<='		{ TOK_LESS_OR_EQUAL }
'>='		{ TOK_GREATER_OR_EQUAL }
'='			{ TOK_EQUALS_TO }
'<'			{ TOK_LESS } 
'>' 		{ TOK_GREATER }
'!'         { TOK_NOT }
'&&'        { TOK_AND }
'||'        { TOK_OR }

%left '&&' '||'
%nonassoc '==' '!=' '<=' '>=' '=' '<' '>'
%left '+' '-'
%left '*' '/' '%'
%right '!'

%%

Code : Func      { [$1] }
     | Code Func { $1 ++ [$2] } 

Func : int main '(' ')' '{' Stms '}'              { MainFunc $6 }
     | Type string '(' ')' '{' Stms Ret '}'       { Fnc $1 $2 $6 $7 }
     | Type string '(' Argms ')' '{' Stms Ret '}' { FncAr $1 $2 $4 $7 $8 }

Argms : Argm           { [$1] }
      | Argms ',' Argm { $1 ++ [$3] } 

Argm : Type string { Arg $1 $2 }

Ret : return Exp ';' { RtrnFunc $2 }
    | return ';'     { RtrnVoid }

Stms :          {[]} 
     | Stms Stm { $1 ++ [$2] } 

Stm : Atr                              { AtDe $1 }
    | '{' Stms '}'                     { Bloc $2 }
    | if '(' Exp_Bool ')' Stm          { If $3 $5 Skip Skip }
    | if '(' Exp_Bool ')' Stm else Stm { If $3 $5 Else  $7  }
    | print_int '(' Exp ')' ';'        { PrtInt $3 }
    | while '(' Exp_Bool ')' Stm       { While $3 $5 }
    | string '(' Exps ')'';'           { FuncCallArg $1 $3 }
    | return Exp ';'                   { Rtrn $2 }

Exps :              {[]}
     | Exp          {[$1]}
     | Exps ',' Exp { $1 ++ [$3] } 

Atr : string '=' Exp ';'               { Atrb $1 $3 }
	| Type string '=' Exp ';'          { AtrDecl $1 $2 $4 }
	| Type string ';'                  { Decl $1 $2 }

Exp : num                       { Num $1 }  
    | string                    { Sent $1 }
    | true                      { Boolean True }  
    | false                     { Boolean False }
    | scan_int '(' ')'          { ScnInt }
    | '(' Exp ')'               { $2 }   
    | string '(' Exps ')'       { FcCallArg $1 $3 }
    | Exp '+' Exp               { Add $1 $3 }
    | Exp '-' Exp               { Minus $1 $3 }
    | Exp '*' Exp               { Times $1 $3 }
    | Exp '/' Exp               { Div $1 $3 } 
    | Exp '%' Exp               { Mod $1 $3 }

Exp_Bool : '!' Exp_Bool            { Not $2 } 
         | string '(' Exps ')'     { FCallArg $1 $3 }
         | Exp_Bool '&&' Exp_Bool  { And $1 $3 }
         | Exp_Bool '||' Exp_Bool  { Or $1 $3 }
         | Exp '>' Exp             { Greater $1 $3 }
         | Exp '<' Exp             { Less $1 $3 }
         | Exp '==' Exp            { Equals_to $1 $3 }
         | Exp '!=' Exp            { Different $1 $3 }
         | Exp '>=' Exp            { Greater_Or_Equal $1 $3 }
         | Exp '<=' Exp            { Less_Or_Equal $1 $3 }

Type : int  { TyInt }
     | bool { TyBool }

{

data Code = Func
            deriving Show

data Func = MainFunc [Stm]
          | Fnc Type String [Stm] Ret
          | FncAr Type String [Argm] [Stm] Ret 
          deriving Show

data Argm = Arg Type String
          deriving Show

data Ret = RtrnFunc Exp 
         | RtrnVoid
         deriving Show


data Type = TyBool
          | TyInt
          deriving Show     

data Stm = AtDe Atr
         | If Exp_Bool Stm Stm Stm 
         | Else
         | Skip
         | Bloc [Stm]
         | PrtInt Exp
         | While Exp_Bool Stm
         | FuncCallArg String [Exp] 
         | Rtrn Exp
         deriving Show

data Atr = Atrb String Exp
         | AtrDecl Type String Exp
         | Decl Type String
         deriving Show

data Exp = Num Int 
         | Sent String
         | Boolean Bool
         | ScnInt
         | Add Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | FcCallArg String [Exp]
         deriving Show

data Exp_Bool = Greater Exp Exp
              | Less Exp Exp
              | Equals_to Exp Exp
              | Different Exp Exp
              | Greater_Or_Equal Exp Exp
              | Less_Or_Equal Exp Exp
              | Not Exp_Bool
              | And Exp_Bool Exp_Bool
              | Or Exp_Bool Exp_Bool
              | FCallArg String [Exp]
              deriving Show 

parseError :: [Token] -> a
parseError toks = error "parse error"
}