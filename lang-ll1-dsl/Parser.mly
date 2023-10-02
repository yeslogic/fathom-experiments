%token <int> INT
%token <string> NAME

%token KEWORD_DEF "def"

%token BANG "!"
%token COLON_EQUALS ":="
%token COMMA ","
%token PIPE "|"
%token SEMI ";"

%token DOT_DOT ".."
%token DOT_DOT_LESS "..<"
%token GREATER_DOT_DOT ">.."
%token GREATER_DOT_DOT_LESS ">..<"

%token LBRACE "{"
%token RBRACE "}"
%token LPAREN "("
%token RPAREN ")"

%token END

%start <Surface.program> program

%%

let program :=
  | is = list(item); END;
      { Surface.{ items =  is } }

let item :=
  | "def"; n = NAME; ":="; t = term; ";";
      { n, t }

let term :=
  | "|"; t0 = cat_term; "|"; t1 = alt_term;
      { Surface.Alt (t0, t1) }
  | t0 = cat_term; "|"; t1 = alt_term;
      { Surface.Alt (t0, t1) }
  | cat_term

let alt_term :=
  | t0 = cat_term; "|"; t1 = alt_term;
      { Surface.Alt (t0, t1) }
  | cat_term

let cat_term :=
  | t0 = atomic_term; ","; t1 = cat_term;
      { Surface.Cat (t0, t1) }
  | atomic_term

let atomic_term :=
  | n = NAME;
      { Name n }
  | "("; ")";
      { Unit }
  | "("; t = term; ")";
      { t }
  | "{"; i = INT; "}";
      { Surface.Byte (Pos, i) }
  | "!"; "{"; i = INT; "}";
      { Surface.Byte (Neg, i) }
  | "{"; r = range; "}";
      { Surface.ByteRange (Pos, r) }
  | "!"; "{"; r = range; "}";
      { Surface.ByteRange (Neg, r) }

let range :=
  | start = option(inclusive); ".."; stop = option(inclusive); { Surface.{ start; stop } }
  | start = option(inclusive); "..<"; stop = some(exclusive); { Surface.{ start; stop } }
  | start = some(exclusive); ">.."; stop = option(inclusive); { Surface.{ start; stop } }
  | start = some(exclusive); ">..<"; stop = some(exclusive); { Surface.{ start; stop } }

let inclusive == i = INT; { Surface.Inclusive i }
let exclusive == i = INT; { Surface.Exclusive i }

let some(A) ==
  x = A; { Some x }
