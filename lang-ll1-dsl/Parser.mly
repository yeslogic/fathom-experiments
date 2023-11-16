%token <int> INT
%token <string> NAME

%token KEWORD_DEF "def"

%token BANG "!"
%token COLON ":"
%token COLON_EQUALS ":="
%token COMMA ","
%token EQUALS_GREATER "=>"
%token FULL_STOP "."
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
  | "def"; n = NAME; t0 = option(":"; t = tm; { t }); ":="; t1 = tm; ";";
      { n, t0, t1 }

let tm :=
  | "|"; t = union_tm;
      { t }
  | union_tm

let union_tm :=
  | t0 = seq_tm; "|"; t1 = union_tm;
      { Surface.union t0 t1 }
  | seq_tm

let seq_tm :=
  | t0 = range_tm; ","; t1 = seq_tm;
      { Surface.seq t0 t1 }
  | range_tm

let range_tm :=
  | t = range_tm; "{"; n = NAME; "=>"; b = tm; "}"; { Surface.action t (n, b) }
  | start = inclusive; ".."; stop = inclusive; { Surface.range start stop }
  | start = inclusive; "..<"; stop = exclusive; { Surface.range start stop }
  | start = exclusive; ">.."; stop = inclusive; { Surface.range start stop }
  | start = exclusive; ">..<"; stop = exclusive; { Surface.range start stop }
  | proj_tm

let proj_tm :=
  | t = proj_tm; "."; l = NAME;
      { Surface.proj t l }
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
