%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug

%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
%x X_STRING X_LINE_COMMENT X_COMMENT X_CHAR_SPECIAL X_IGNORE_AFTER_SLASH_ZERO

ID 						            [a-zA-Z][0-9a-zA-Z]*
W_SPACE      					    [ \n\t\r]+
DIGIT							        [0-9]
INTEGER_10							  [1-9]{DIGIT}*|0
INTEGER_8                 0[0-7]+
EXPONENT          		    [eE][-+]?{DIGIT}+
DOUBLE							      {DIGIT}+{EXPONENT}|{DIGIT}+"."{DIGIT}*{EXPONENT}?|{DIGIT}*"."{DIGIT}+{EXPONENT}?

%%
  yydebug=1; set_debug(1);

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 
"||"                   return tOR;
"&&"                   return tAND;

"!"				   			     return tPRINT;
"!!"                   return tPRINTNL;

"int"                  return tTYPEINT;
"double"               return tTYPEDOUBLE;
"string"               return tTYPESTRING;
"void"                 return tVOID;
"null"                 return tNULLPTR;
"auto"                 return tAUTO;

"if"                   return tIF; 
"else"                 return tELSE;
"elif"                 return tELIF;

"while"                return tWHILE;
"sizeof"               return tSIZEOF;
"next"                 return tNEXT;
"stop"                 return tSTOP;
"return"               return tRETURN;
"input"                return tREAD; 
"begin"                return tBEGIN; 
"end"                  return tEND; 

"foreign"              return tFOREIGN;
"forward"              return tFORWARD;
"public"               return tPUBLIC;

{ID}          { yylval.s = new std::string(yytext); return tIDENTIFIER; }

  /* ============================[COMENTARIOS]============================ */

"//".*$                 { ; }

"/*"                          { yy_push_state(X_COMMENT); }
<X_COMMENT>"/*"               { yy_push_state(X_COMMENT); }
<X_COMMENT>"*/"               { yy_pop_state(); }
<X_COMMENT>.|\n               { ; /* ignore content */ }


  /* ============================[INTEIROS]============================ */


{INTEGER_10}            { yylval.i = strtol(yytext, nullptr, 10);
                          if(errno == ERANGE) yyerror("Conversion results in overflow."); 
                          return tINTEGER; }

{INTEGER_8}             { yylval.i = strtol(yytext, nullptr, 8);	
                          if(errno == ERANGE) yyerror("Conversion results in overflow."); 
                          return tINTEGER; }

  /* ============================[DOUBLES]============================ */

{DOUBLE}                { yylval.d = strtod(yytext, nullptr);
                          if(errno == ERANGE) yyerror("Conversion results in overflow."); 
                          return tDOUBLE; }

  /* ============================[STRINGS]============================ */

"\""                     { yy_push_state(X_STRING); yylval.s = new std::string(""); }
<X_STRING>"\""           { yy_pop_state(); return tSTRING; }
<X_STRING>"\\"           { yy_push_state(X_CHAR_SPECIAL); }
<X_STRING>.              { *yylval.s += yytext; }
<X_STRING>\n             { yyerror("newline in string"); }

<X_CHAR_SPECIAL>t                       { *yylval.s += "\t"; yy_pop_state(); }
<X_CHAR_SPECIAL>n                       { *yylval.s += "\n"; yy_pop_state(); }
<X_CHAR_SPECIAL>r                       { *yylval.s += "\r"; yy_pop_state(); }
<X_CHAR_SPECIAL>0                       { yy_pop_state(); yy_pop_state(); yy_push_state(X_IGNORE_AFTER_SLASH_ZERO); }
<X_CHAR_SPECIAL>"\""                    { *yylval.s += "\""; yy_pop_state(); }
<X_CHAR_SPECIAL>"\\"			              { *yylval.s += "\\"; yy_pop_state(); }
<X_CHAR_SPECIAL>[0-7]{1,3}              { *yylval.s += std::string(1, strtol(yytext, nullptr, 8)); yy_pop_state(); }
<X_CHAR_SPECIAL>.			                  { yyerror("Unknown char"); }

<X_IGNORE_AFTER_SLASH_ZERO>"\""         { yy_pop_state(); return tSTRING; }
<X_IGNORE_AFTER_SLASH_ZERO>"\\\\"       { ; }
<X_IGNORE_AFTER_SLASH_ZERO>"\\\""       { ; }
<X_IGNORE_AFTER_SLASH_ZERO>.|\n         { ; }

[-()\]<>=+*?[/%;~{}.@,]       { return *yytext; }

{W_SPACE}              { ; /* ignore whitespace */ }

.                      { yyerror("Unknown character"); }

%%
