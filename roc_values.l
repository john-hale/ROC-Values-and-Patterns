%{
#include "roc_parser.tab.h"
#include <string.h>
%}
%option noyywrap yylineno
NAME    [a-z][a-zA-Z0-9_]*
AGENT   [A-Z][a-zA-Z0-9_]*
WS      [ \t\n\r]+
%%
^"#"[^\n]*\n    { }              /* skip # comment lines (e.g. from roc_gen) */
{WS}            { }
"nil"           { return NIL; }
{NAME}"?"       { yylval.sval=strndup(yytext,yyleng-1); return WILDCARD; }
"#"             { return BINDABLE; }
{NAME}          { yylval.sval=strdup(yytext); return NAME; }
{AGENT}         { yylval.sval=strdup(yytext); return AGENT; }
"["             { return LBRACKET; }
"]"             { return RBRACKET; }
","             { return COMMA; }
.               { fprintf(stderr,"Unexpected char '%s' line %d\n",yytext,yylineno); }
%%
