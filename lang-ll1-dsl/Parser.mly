%token <int> INT
%token <string> NAME

%token KEWORD_DEF "def"

%token BANG "!"
%token COLON_EQUALS ":="
%token COMMA ","
%token EQUALS_GREATER "=>"
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
  | "|"; t = alt_tm;
      { t }
  | t0 = cat_tm; "|"; t1 = alt_tm;
      { Surface.alt t0 t1 }
  | cat_tm

let alt_tm :=
  | t0 = cat_tm; "|"; t1 = alt_tm;
      { Surface.alt t0 t1 }
  | cat_tm

let cat_tm :=
  | t0 = range_tm; ","; t1 = cat_tm;
      { Surface.cat t0 t1 }
  | range_tm

let range_tm :=
  | t = range_tm; "{"; n = NAME; "=>"; b = tm; "}"; { Surface.action t (n, b) }
  | start = inclusive; ".."; stop = inclusive; { Surface.range start stop }
  | start = inclusive; "..<"; stop = exclusive; { Surface.range start stop }
  | start = exclusive; ">.."; stop = inclusive; { Surface.range start stop }
  | start = exclusive; ">..<"; stop = exclusive; { Surface.range start stop }
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
  | i = INT;
      { Surface.int i }

let inclusive ==
  | t = atomic_tm; { Surface.Inclusive t }
  | { Surface.Open }

let exclusive ==
  | t = atomic_tm; { Surface.Exclusive t }
