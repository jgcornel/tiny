/*---------------------------------------------------------------------------//
//                                                                           //
// tiny.l                                                                    //
// Author: Jan Cornelis                                                      //
// Date  : July 2009                                                         //
//                                                                           //
// Provides the lexer specification for our tiny compiler.                   //
//                                                                           //
//---------------------------------------------------------------------------*/

%{

#include    "tiny.H" 

extern int lineno;

%}

alpha       [_A-Za-z]
digit       [0-9]
alphanum    [_A-Za-z0-9]

%%
[ \t\r]             break;
"\n"                ++lineno;

int                 return INT;
char                return CHAR;
if                  return IF;
else                return ELSE;
while               return WHILE;
read                return READ;
write               return WRITE;
return              return RETURN;
length              return LENGTH;

{alpha}{alphanum}*  return IDENTIFIER;
{digit}+            {
                      yylval = atoi(yytext);
                      return NUMBER;
                    }
'.'                 return QCHAR;

"("                 return LPAR;
")"                 return RPAR;
"{"                 return LBRACE;
"}"                 return RBRACE;
"["                 return LBRACK;
"]"                 return RBRACK;
"="                 return ASSIGN;
";"                 return SEMICOLON;
","                 return COMMA;
"+"                 return PLUS;
"-"                 return MINUS;
"*"                 return TIMES;
"/"                 return DIVIDE;
"=="                return EQUAL;
"!="                return NEQUAL;
">"                 return GREATER;
"<"                 return LESS;
"!"                 return NOT;

%%
