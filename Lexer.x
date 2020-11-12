{
 module Lexer where
}

%wrapper "basic"

$alpha = [_a-zA-Z]
$digit = [0-9]
$white = [\ \ t\n\r]

tokens:-

$white+;				--ignorar carateres brancos
int                 { \_ -> TOK_INT }
return              { \_ -> TOK_RETURN }
print_int           { \_ -> TOK_PRINT_INT }
while 				{ \_ -> TOK_WHILE }
true 				{ \_ -> TOK_TRUE }
false				{ \_ -> TOK_FALSE }
bool                { \_ -> TOK_BOOL }
if                  { \_ -> TOK_IF }
$alpha($alpha|$digit)*  { \s -> ID s}
$digit+             { \s -> TOK_NUM (read s) }            
"+"					{ \_ -> TOK_PLUS }
"*"					{ \_ -> TOK_TIMES }
"-"					{ \_ -> TOK_MINUS }
"/"					{ \_ -> TOK_DIV }
"%"					{ \_ -> TOK_MOD }
";"					{ \_ -> TOK_SEMICOLON }
"("					{ \_ -> TOK_LPAREN }
")"					{ \_ -> TOK_RPAREN }
"}"					{ \_ -> TOK_RBRACE }
"{"	                { \_ -> TOK_LBRACE }
"=="				{ \_ -> TOK_EQUAL }
"!="				{ \_ -> TOK_DIFFERENT }
">="				{ \_ -> TOK_LESS_OR_EQUAL }
"<="				{ \_ -> TOK_GREATER_OR_EQUAL }
"="					{ \_ -> TOK_EQUALS_TO }
"<"					{ \_ -> TOK_LESS } 
">"					{ \_ -> TOK_GREATER }

{
data Token = TOK_IF
           | ID String
           | TOK_NUM Int
           | TOK_INT
           | TOK_BOOL
           | TOK_PLUS
           | TOK_TIMES
           | TOK_MINUS
           | TOK_DIV
           | TOK_MOD
           | TOK_LPAREN 
           | TOK_RPAREN 
           | TOK_RBRACE 
           | TOK_LBRACE 
           | TOK_SEMICOLON 
           | TOK_EQUALS_TO 
           | TOK_LESS 
           | TOK_GREATER 
           | TOK_EQUAL 
           | TOK_DIFFERENT 
           | TOK_LESS_OR_EQUAL 
           | TOK_GREATER_OR_EQUAL 
           | TOK_TRUE
           | TOK_FALSE
           | TOK_RETURN
           | TOK_PRINT_INT
           | TOK_WHILE
           deriving (Eq,Show)
}