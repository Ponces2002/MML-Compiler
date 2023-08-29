%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;     /* double value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  cdk::typed_node      *typednode;
  mml::block_node      *block;
  mml::program_node    *program;
  std::vector<std::shared_ptr<cdk::basic_type>> *types;
};

%token <i> tINTEGER
%token <s> tIDENTIFIER tSTRING
%token <d> tDOUBLE
%token tTYPEINT tTYPEDOUBLE tTYPESTRING tVOID tNULLPTR tAUTO
%token tWHILE tIF tPRINT tPRINTNL tBEGIN tEND
%token tNEXT tSTOP tRETURN tREAD tSIZEOF
%token tPUBLIC tFOREIGN tFORWARD tPRIVATE

%nonassoc tIFX
%nonassoc tELIF tELSE

%right '='
%left tOR
%left tAND
%nonassoc '~'
%left tEQ tNE
%left tGE tLE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '(' ')' '[' ']'

%type <node> stmt elif decl file
%type <sequence> list exprs decls funcArgs params
%type <expression> expr funcDef funcCall
%type <lvalue> lval
%type <s> string
%type <type> type funcType
%type <typednode> variable funcArg
%type <block> block
%type <types> types
%type <program> main

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file  : decls main   { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
      | decls        { compiler->ast($$ = $1); }  
      | main         { compiler->ast(new cdk::sequence_node(LINE, $1)); }
      ;  

main : tBEGIN block tEND    { $$ = new mml::program_node(LINE, $2); }
     ;

block : decls      { $$ = new mml::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
      | list       { $$ = new mml::block_node(LINE, new cdk::sequence_node(LINE), $1); }
      | decls list { $$ = new mml::block_node(LINE, $1, $2); }
      |            { $$ = new mml::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
      ;

decls : decl          { $$ = new cdk::sequence_node(LINE, $1); }
      | decls decl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

list : stmt	     { $$ = new cdk::sequence_node(LINE, $1); }
	| list stmt    { $$ = new cdk::sequence_node(LINE, $2, $1); }
	;

stmt : expr ';'                              { $$ = new mml::evaluation_node(LINE, $1); }
     | tWHILE '(' expr ')' stmt              { $$ = new mml::while_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt %prec tIFX      { $$ = new mml::if_node(LINE, $3, $5); }
     | tIF '(' expr ')' stmt elif            { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     | '{' block '}'                         { $$ = $2; }
     | exprs tPRINT                          { $$ = new mml::print_node(LINE, $1, false); }
     | exprs tPRINTNL                        { $$ = new mml::print_node(LINE, $1, true); }
     | tRETURN expr ';'                      { $$ = new mml::return_node(LINE, $2); }
     | tRETURN funcDef ';'                      { $$ = new mml::return_node(LINE, $2); }
     | tRETURN ';'                           { $$ = new mml::return_node(LINE, nullptr); }
     | tNEXT ';'                             { $$ = new mml::next_node(LINE); }
     | tNEXT tINTEGER ';'                    { $$ = new mml::next_node(LINE, $2); }
     | tSTOP ';'                             { $$ = new mml::stop_node(LINE); }
     | tSTOP tINTEGER ';'                    { $$ = new mml::stop_node(LINE, $2); }
     ;

elif : tELSE stmt                                 { $$ = $2; }
     | tELIF '(' expr ')' stmt %prec tIFX         { $$ = new mml::if_node(LINE, $3, $5); }
     | tELIF '(' expr ')' stmt elif               { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     ;

exprs : expr                   { $$ = new cdk::sequence_node(LINE, $1); }
      | funcDef                { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs ',' expr         { $$ = new cdk::sequence_node(LINE, $3, $1); }
      | exprs ',' funcDef      { $$ = new cdk::sequence_node(LINE, $3, $1); }
      ;

expr : tINTEGER                   { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLE                    { $$ = new cdk::double_node(LINE, $1); }
	| string                     { $$ = new cdk::string_node(LINE, $1); }
     | tNULLPTR                   { $$ = new mml::nullptr_node(LINE); }
     | tREAD                      { $$ = new mml::input_node(LINE); }
     | '-' expr %prec tUNARY      { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUNARY      { $$ = $2; }
     | '~' expr                   { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr	         { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	         { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	         { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	         { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	         { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr	         { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	         { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	         { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr              { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr	         { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr	         { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tAND expr             { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR expr              { $$ = new cdk::or_node(LINE, $1, $3); }
     | '(' expr ')'               { $$ = $2; }
     | lval                       { $$ = new cdk::rvalue_node(LINE, $1); }  //FIXME
     | lval '=' expr              { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '=' funcDef           { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '?'                   { $$ = new mml::address_of_node(LINE, $1); }
     | tSIZEOF '(' expr ')'       { $$ = new mml::sizeof_node(LINE, $3); }
     | funcCall                   { $$ = $1; }
     | '[' expr ']'               { $$ = new mml::stack_alloc_node(LINE, $2); }
     ;

string : string tSTRING        { $$ = new std::string(*$1 + *$2); delete $1; delete $2; }
       | tSTRING               { $$ = $1; }
       ;

lval : tIDENTIFIER                { $$ = new cdk::variable_node(LINE, $1); }
     | expr '[' expr ']'          { $$ = new mml::index_node(LINE, $1, $3); }
     ;

decl : variable ';'            { $$ = $1; }
     ;

variable : type tIDENTIFIER                                     { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
         | type tIDENTIFIER '=' expr                            { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
         | tPUBLIC type tIDENTIFIER '=' expr                    { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $5); }
         | tPUBLIC tAUTO tIDENTIFIER '=' expr                   { $$ = new mml::declaration_node(LINE, tPUBLIC, nullptr, *$3, $5); }
         | tPUBLIC tIDENTIFIER '=' funcDef                      { $$ = new mml::declaration_node(LINE, tPUBLIC, nullptr, *$2, $4); }
         | tAUTO tIDENTIFIER '=' expr                           { $$ = new mml::declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); }
         | tAUTO tIDENTIFIER '=' funcDef                        { $$ = new mml::declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); }
         | tFORWARD type tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFORWARD, $2, *$3, nullptr); }
         | tFOREIGN type tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); }
         | type tIDENTIFIER '=' funcDef                         { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
         ;

funcDef : '(' params ')' '-' '>' type '{' block '}'    { $$ = new mml::function_definition_node(LINE, $6, $2, $8); }
        ;

params : funcArgs                               { $$ = $1; }
       |                                        { $$ = new cdk::sequence_node(LINE); }
       ;

funcArgs : funcArg                              { $$ = new cdk::sequence_node(LINE, $1); }                
         | funcArgs ',' funcArg                 { $$ = new cdk::sequence_node(LINE, $3, $1); }
         ;

funcArg : type tIDENTIFIER                     { $$ = new mml::declaration_node(LINE, 0, $1, *$2, nullptr); }
        ;

funcCall : expr '(' exprs ')'                        { $$ = new mml::function_call_node(LINE, $1, $3); }
         | expr '(' ')'                              { $$ = new mml::function_call_node(LINE, $1, new cdk::sequence_node(LINE)); }
         | '@' '(' exprs ')'                         { $$ = new mml::function_call_node(LINE, nullptr, $3); }
         | '@' '(' ')'                               { $$ = new mml::function_call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
         | '(' funcDef ')' '(' exprs ')'             { $$ = new mml::function_call_node(LINE, $2, $5); }
         | '(' funcDef ')' '(' ')'                   { $$ = new mml::function_call_node(LINE, $2, new cdk::sequence_node(LINE)); }
         ;

type : tTYPESTRING                       { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | tTYPEINT                          { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tTYPEDOUBLE                       { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tVOID                             { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | '[' type ']'                      { $$ = cdk::reference_type::create(4, std::shared_ptr<cdk::basic_type>($2)); }
     | funcType                          { $$ = $1; }
     ;

funcType  : type '<' types '>'          { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
          ;

types     : type                        { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
          | types ',' type              { $$ = $1; $$->push_back($3); }
          |                             { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); }
          ;

%%
