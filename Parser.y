{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token	

if			{ TOK_IF }
int 		{ TOK_INT }
bool        { TOK_BOOL }
true 		{ TOK_TRUE }
false		{ TOK_FALSE }
return		{ TOK_RETURN }
print_int	{ TOK_PRINT_INT}
while 		{ TOK_WHILE }
num 		{ TOK_NUM $$ }
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

Atr : string '=' Exp ';' { Atr_string $1 $3 }
	| int string '=' Exp ';' { Atr_int_value $2 $4 }
	| int string ';' { Atr_int $2 }
	| bool string '=' Exp ';' { Atr_boolean_value $2 $4 }
	| bool string ';' { Atr_boolean $2 } 

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
    | Exp '>' Exp { Greater $1 $3 }
    | Exp '<' Exp { Less $1 $3 }
    | Exp '==' Exp { Equals_to $1 $3 }
    | Exp '!=' Exp { Different $1 $3 }
    | Exp '>=' Exp { Greater_Or_Equal $1 $3 }
    | Exp '<=' Exp { Less_Or_Equal $1 $3 }

{
data Atr = Atr_string String Exp
         | Atr_int_value String Exp
         | Atr_int String
         | Atr_boolean_value String Exp
         | Atr_boolean String
         deriving Show

data Exp = Num Int 
         | Sent String
         | Add Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Boolean Bool
         | Greater Exp Exp
         | Less Exp Exp
         | Equals_to Exp Exp
         | Different Exp Exp
         | Greater_Or_Equal Exp Exp
         | Less_Or_Equal Exp Exp 
         deriving Show

parseError :: [Token] -> a
parseError toks = error "parse error"
}