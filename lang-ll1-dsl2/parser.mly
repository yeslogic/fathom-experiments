%token <int> INT
%token <string> NAME

%token KEYWORD_FORMAT "format"
%token KEYWORD_TYPE "type"
%token KEYWORD_DEF "def"

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
  | items = list(item); END;
      { items }

let item :=
  | "format"; n = located(NAME); ":="; f = located(tm); ";";
      { Surface.Format_def (n, f) }
  | "type"; n = located(NAME); ":="; t = located(tm); ";";
      { Surface.Type_def (n, t) }
  | "def"; n = located(NAME); t = option(":"; ~ = located(tm); <>); ":="; e = located(tm); ";";
      { Surface.Def (n, t, e) }

let tm :=
  | "|"; t = union_tm;
      { t }
  | tm1 = located(union_tm); ":"; tm2 = located(tm);
      { Surface.Ann (tm1, tm2) }
  | union_tm

let union_tm :=
  | f1 = located(range_tm); "|"; f2 = located(union_tm);
      { Surface.Op2 (`Or, f1, f2) }
  | range_tm

let range_tm :=
  | f = located(proj_tm); "{"; n = located(NAME); "=>"; b = located(tm); "}";
      { Surface.Action_format (f, (n, b)) }
  | start = inclusive; ".."; stop = inclusive; { Surface.Range_format (start, stop) }
  | start = inclusive; "..<"; stop = exclusive; { Surface.Range_format (start, stop) }
  | start = exclusive; ">.."; stop = inclusive; { Surface.Range_format (start, stop) }
  | start = exclusive; ">..<"; stop = exclusive; { Surface.Range_format (start, stop) }
  | proj_tm

let proj_tm :=
  | e = located(proj_tm); "."; l = located(NAME);
      { Surface.Proj (e, `Label l) }
  | e = located(proj_tm); "."; i = located(INT);
      { Surface.Proj (e, `Index i) }
  | atomic_tm

let atomic_tm :=
  | "("; e = tm; ")";
      { e }
  | n = NAME;
      { Surface.Name n }
  | i = INT;
      { Surface.Int i }
  | "!"; f = located(atomic_tm);
      { Surface.Op1(`Not, f) }
  | "{"; "}";
      { Surface.Record_empty }
  | "{"; fs = trailing_nonempty_list(";", ~ = located(NAME); ":"; ~ = located(tm); <>); "}";
      { Surface.Record_ty fs }
  | "{"; fs = trailing_nonempty_list(";", ~ = located(NAME); ":="; ~ = located(tm); <>); "}";
      { Surface.Record_lit fs }
  | "{"; fs = trailing_nonempty_list(";", ~ = located(NAME); "<-"; ~ = located(tm); <>); "}";
      { Surface.Record_format fs }
  | "("; ")";
      { Surface.Tuple [] }
  | "("; t = located(tm); ","; ts = trailing_list(",", located(tm)); ")";
      { Surface.Tuple (t :: ts) }

let inclusive ==
  | e = located(INT); { Surface.Inclusive e }
  | { Surface.Open }

let exclusive ==
  | e = located(INT); { Surface.Exclusive e }

// Utilities

let located(X) :=
  | data = X;
      { Surface.{ loc = $loc; data } }

let trailing_list(Sep, T) :=
  | { [] }
  | trailing_nonempty_list(Sep, T)

let trailing_nonempty_list(Sep, T) :=
  | t = T; option(Sep);
      { [ t ] }
  | t = T; Sep; ts = trailing_nonempty_list(Sep, T);
      { t :: ts }
