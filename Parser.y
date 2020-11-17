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

%nonassoc '==' '!=' '<=' '>=' '=' '<' '>'
%left '+' '-'
%left '*' '/'
%left '%'

%%

Func : int main '(' ')' '{' Stms '}'                { Main $6 }
     | Type string '(' FDe ')' '{' Stms FRe ';' '}' { Fnc $1 $2 $4 $7 $8 }

FDe : {[]}
    | Type string {[($1,$2)]}
    | FDe ',' Type string { $1 ++ [($3,$4)] }

FRe : {[]} 
    | return Exp           { RtE  $2}
    | return Exp_Bool      { RtEB $2}
    | return FCa           { RtFuncCall  $2}

FCa : string '(' Exp ')' { $1 $3 }

Stms : {[]} 
     | Stms Stm { $1 ++ [$2] } 

Stm : Atr { AtDe $1 }
    | '{' Stms '}' { Bloc $2 }
    | if '(' Exp_Bool ')' Stm          { If $3 $5 Skip Skip }
    | if '(' Exp_Bool ')' Stm else Stm { If $3 $5 Else  $7  }
    | while '(' Exp_Bool ')' Stm       { While $3 $5 }
    | FCa ';'                          {}
    | FRe ';'                          {}


Atr : string '=' Exp ';' { Atrb $1 $3 }
	| Type string '=' Exp ';' { AtrDecl $1 $2 $4 }
	| Type string ';' { Decl $1 $2 }


Exp : num { Num $1 }  
    | string { Sent $1 }
    | true { Boolean True }
    | false { Boolean False }
    | '(' Exp ')' { $2 }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '-' Exp { Minus $1 $3 }
    | Exp '*' Exp { Times $1 $3 }
    | Exp '/' Exp { Div $1 $3 } 
    | Exp '%' Exp { Mod $1 $3 }

Exp_Bool : Exp '>' Exp { Greater $1 $3 }
         | Exp '<' Exp { Less $1 $3 }
         | Exp '==' Exp { Equals_to $1 $3 }
         | Exp '!=' Exp { Different $1 $3 }
         | Exp '>=' Exp { Greater_Or_Equal $1 $3 }
         | Exp '<=' Exp { Less_Or_Equal $1 $3 }

Type : int  { TyInt }
     | bool { TyBool }

{
data Func = Main Stm 
          | Fnc Type String FDe Stm FRe

data Type = TyBool
          | TyInt
          deriving Show     

data Stm = AtDe Atr
         | If Exp_Bool Stm Stm Stm 
         | Else
         | Skip
         | Bloc [Stm]
         | While Exp_Bool Stm
         | FuncCall String
         | Return Exp
         deriving Show

data Atr = Atrb String Exp
         | AtrDecl Type String Exp
         | Decl Type String
         deriving Show

data Exp = Num Int 
         | Sent String
         | Boolean Bool
         | Add Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         deriving Show

data Exp_Bool = Greater Exp Exp
              | Less Exp Exp
              | Equals_to Exp Exp
              | Different Exp Exp
              | Greater_Or_Equal Exp Exp
              | Less_Or_Equal Exp Exp
              deriving Show 

parseError :: [Token] -> a
parseError toks = error "parse error"
}