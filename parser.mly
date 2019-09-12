%{

open Lang

%}
%token <string> NAME

%token DEFINE

%token EQUAL

%token SEMI

%start <prog> program

%%

located(p):
  out=p { Loc(Span( }

synth:
  x=NAME { Var x }

decl:
  DEFINE x=NAME EQUAL s=synth { Def (x, s) }

program:
  ds = decl* SEMI s = synth { Prog (ds, s) }
