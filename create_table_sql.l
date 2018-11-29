%{
#include "pmysql.tab.h"
#include <stdarg.h>
#include <string.h>

void yyerror(char *s, ...);

int oldstate;

%}

%option noyywrap yylineno case-insensitive
%x COMMENT
%s BTWMODE

%%	
AUTO_INCREMENT	{ return AUTO_INCREMENT; }
BIT	{ return BIT; }
CHANGE	{ return CHANGE; }
COLLATE	{ return COLLATE; }
COLUMN	{ return COLUMN; }
COMMENT	{ return COMMENT; }
CREATE	{ return CREATE; }
DATE	{ return DATE; }
DATETIME	{ return DATETIME; }
DECLARE	{ return DECLARE; }
DEFAULT	{ return DEFAULT; }
EXISTS	{ yylval.subtok = 0; return EXISTS; }
FULLTEXT	{ return FULLTEXT; }
IF	{ return IF; }
NOT	{ return NOT; }
NUMBER	{ return NUMBER; }
PRIMARY	{ return PRIMARY; }
SET	{ return SET; }
TABLE	{ return TABLE; }
TEMPORARY	{ return TEMPORARY; }
TIME	{ return TIME; }
TIMESTAMP	{ return TIMESTAMP; }
UNIQUE	{ return UNIQUE; }
UNSIGNED	{ return UNSIGNED; }
YEAR	{ return YEAR; }
ZEROFILL	{ return ZEROFILL; }
INDEX|KEY	{ return KEY; }
VARCHAR(ACTER)?   { return VARCHAR; }
NULL	{ return NULLX; }
INT4?|INTEGER	{ return INTEGER; }
FLOAT8|DOUBLE	{ return DOUBLE; }
FLOAT4?	{ return FLOAT; }
NUMERIC|DEC|DECIMAL	{ return DECIMAL; }
CHAR(ACTER)?	{ return CHAR; }
[A-Za-z][A-Za-z0-9]*	{yylval.strval = strdup(yytext);return NAME;}
    /* strings */

'(\\.|''|[^'\n])*'	|
\"(\\.|\"\"|[^"\n])*\"  { yylval.strval = strdup(yytext); return STRING; }
'(\\.|[^'\n])*$	        { yyerror("Unterminated string %s", yytext); }
\"(\\.|[^"\n])*$	{ yyerror("Unterminated string %s", yytext); }
   /* hex strings */
X'[0-9A-F]+' |  
0X[0-9A-F]+  { yylval.strval = strdup(yytext); return STRING; }
   /* bit strings */
0B[01]+      |
B'[01]+'     { yylval.strval = strdup(yytext); return STRING; }

-?[0-9]+	        { yylval.intval = atoi(yytext); return INTNUM; } 

-?[0-9]+"."[0-9]* |
-?"."[0-9]+	|
-?[0-9]+E[-+]?[0-9]+	|
-?[0-9]+"."[0-9]*E[-+]?[0-9]+ |
-?"."[0-9]*E[-+]?[0-9]+	{ yylval.floatval = atof(yytext) ;
                                  return APPROXNUM; }
@[0-9a-z_.$]+ |
@\"[^"\n]+\" |
@`[^`\n]+` |
@'[^'\n]+' { yylval.strval = strdup(yytext+1); return USERVAR; }
	/*anything else*/
[ \t\n]         ;/* white space */
.               {return yytext[0];}
%%

