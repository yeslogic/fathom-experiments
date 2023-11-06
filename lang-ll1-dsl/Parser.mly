%token <int> INT
%token <string> NAME

%token KEWORD_DEF "def"

%token BANG "!"
%token COLON ":"
%token COLON_EQUALS ":="
%token COMMA ","
%token EQUALS_GREATER "=>"
%token FULL_STOP "."
%token LESS_HYPHEN "<-"
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
  | "def"; n = NAME; t0 = option(":"; t = located(tm); { t }); ":="; t1 = located(tm); ";";
      { n, t0, t1 }

let tm :=
  | "|"; t = union_tm;
      { t }
  | union_tm

let union_tm :=
  | t0 = located(seq_tm); "|"; t1 = located(union_tm);
      { Surface.union t0 t1 }
  | seq_tm

let seq_tm :=
  | t0 = located(range_tm); ","; t1 = located(seq_tm);
      { Surface.seq t0 t1 }
  | range_tm

let range_tm :=
  | t = located(proj_tm); "{"; n = NAME; "=>"; b = located(tm); "}"; { Surface.action t (n, b) }
  | start = inclusive; ".."; stop = inclusive; { Surface.range start stop }
  | start = inclusive; "..<"; stop = exclusive; { Surface.range start stop }
  | start = exclusive; ">.."; stop = inclusive; { Surface.range start stop }
  | start = exclusive; ">..<"; stop = exclusive; { Surface.range start stop }
  | proj_tm

let proj_tm :=
  | t = located(proj_tm); "."; l = NAME;
      { Surface.proj t l }
  | atomic_tm

let atomic_tm :=
  | n = NAME;
      { Surface.name n }
  | "("; ")";
      { Surface.empty }
  | "("; t = tm; ")";
      { t }
  | "{"; "}";
      { Surface.record_empty }
  | "{"; fs = trailing_nonempty_list(";", field_format); "}";
      { Surface.record_format fs }
  | "{"; fs = trailing_nonempty_list(";", field_decl); "}";
      { Surface.record_ty fs }
  | "{"; fs = trailing_nonempty_list(";", field_defn); "}";
      { Surface.record_lit fs }
  | "!"; t = located(atomic_tm);
      { Surface.not t }
  | i = INT;
      { Surface.int i }

let inclusive ==
  | t = located(atomic_tm); { Surface.Inclusive t }
  | { Surface.Open }

let exclusive ==
  | t = located(atomic_tm); { Surface.Exclusive t }

let located(X) :=
| data = X;
    { Surface.located $loc data }

let field_format ==
  | l = NAME; "<-"; t = located(tm); { l, t }

let field_decl ==
  | l = NAME; ":"; t = located(tm); { l, t }

let field_defn ==
  | l = NAME; ":="; t = located(tm); { l, t }

let trailing_nonempty_list(Sep, T) :=
| t = T; option(Sep);
    { [ t ] }
| t = T; Sep; ts = trailing_nonempty_list(Sep, T);
    { t :: ts }
