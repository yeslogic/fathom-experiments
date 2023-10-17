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
      { Surface.program is }

let item :=
  | "def"; n = NAME; ":="; t = tm; ";";
      { n, t }

let tm :=
  | "|"; t0 = cat_tm; "|"; t1 = alt_tm;
      { Surface.alt t0 t1 }
  | t0 = cat_tm; "|"; t1 = alt_tm;
      { Surface.alt t0 t1 }
  | cat_tm

let alt_tm :=
  | t0 = cat_tm; "|"; t1 = alt_tm;
      { Surface.alt t0 t1 }
  | cat_tm

let cat_tm :=
  | t0 = atomic_tm; ","; t1 = cat_tm;
      { Surface.cat t0 t1 }
  | atomic_tm

let atomic_tm :=
  | n = NAME;
      { Surface.name n }
  | "("; ")";
      { Surface.empty }
  | "("; t = tm; ")";
      { t }
  | "!"; t = atomic_tm;
      { Surface.not t }
  | "{"; i = INT; "}";
      { Surface.byte i }
  | "{"; (start, stop) = range; "}";
      { Surface.byte_range start stop }

let range :=
  | start = option(inclusive); ".."; stop = option(inclusive); { start, stop }
  | start = option(inclusive); "..<"; stop = some(exclusive); { start, stop }
  | start = some(exclusive); ">.."; stop = option(inclusive); { start, stop }
  | start = some(exclusive); ">..<"; stop = some(exclusive); { start, stop }

let inclusive == i = INT; { Surface.Inclusive i }
let exclusive == i = INT; { Surface.Exclusive i }

let some(A) ==
  x = A; { Some x }
