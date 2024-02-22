%token <int> INT
%token <string> NAME

%token KEWORD_FORMAT "format"
%token KEWORD_TYPE "type"
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

%type <Surface.Format.node> format
%type <Surface.Expr.node> expr
%type <Surface.Ty.node> ty

%%

let program :=
  | items = list(item); END;
      { items }

let item :=
  | "format"; n = located(NAME); ":="; f = located(format); ";";
      { Surface.FormatDef (n, f) }
  | "type"; n = located(NAME); ":="; t = located(ty); ";";
      { Surface.TypeDef (n, t) }
  | "def"; n = located(NAME); t = option(":"; ~ = located(ty); <>); ":="; e = located(expr); ";";
      { Surface.ExprDef (n, t, e) }

let ty :=
  | n = NAME;
      { Surface.Ty.Name n }
  | "{"; fs = trailing_list(";", ~ = located(NAME); ":"; ~ = located(ty); <>); "}";
      { Surface.Ty.Record fs }
  | "("; ")";
      { Surface.Ty.Tuple [] }
  | "("; t = located(ty); ","; ts = trailing_list(",", located(ty)); ")";
      { Surface.Ty.Tuple (t :: ts) }

let expr :=
  | e = located(expr); "."; l = located(NAME);
      { Surface.Expr.ProjLabel (e, l) }
  | e = located(expr); "."; i = located(INT);
      { Surface.Expr.ProjIndex (e, i) }
  | atomic_expr

let atomic_expr :=
  | "("; e = expr; ")";
      { e }
  | n = NAME;
      { Surface.Expr.Name n }
  | i = INT;
      { Surface.Expr.IntLit i }
  | "{"; fs = trailing_list(";", ~ = located(NAME); ":="; ~ = located(expr); <>); "}";
      { Surface.Expr.RecordLit fs }
  | "("; ")";
      { Surface.Expr.TupleLit [] }
  | "("; e = located(expr); ","; es = trailing_list(",", located(expr)); ")";
      { Surface.Expr.TupleLit (e :: es) }

let format :=
  | "|"; t = union_format;
      { t }
  | union_format

let union_format :=
  | f1 = located(range_format); "|"; f2 = located(union_format);
      { Surface.Format.Union (f1, f2) }
  | range_format

let range_format :=
  | f = located(atomic_format); "{"; n = located(NAME); "=>"; b = located(expr); "}";
      { Surface.Format.Action (f, (n, b)) }
  | start = inclusive; ".."; stop = inclusive; { Surface.Format.Range (start, stop) }
  | start = inclusive; "..<"; stop = exclusive; { Surface.Format.Range (start, stop) }
  | start = exclusive; ">.."; stop = inclusive; { Surface.Format.Range (start, stop) }
  | start = exclusive; ">..<"; stop = exclusive; { Surface.Format.Range (start, stop) }
  | atomic_format

let atomic_format :=
  | "("; f = format; ")";
      { f }
  | n = NAME;
      { Surface.Format.Name n }
  | i = INT;
      { Surface.Format.Int i }
  | "!"; f = located(atomic_format);
      { Surface.Format.Not f }
  | "{"; fs = trailing_list(";", ~ = located(NAME); "<-"; ~ = located(format); <>); "}";
      { Surface.Format.Record fs }
  | "("; ")";
      { Surface.Format.Tuple [] }
  | "("; f = located(format); ","; fs = trailing_list(",", located(format)); ")";
      { Surface.Format.Tuple (f :: fs) }

let inclusive ==
  | e = located(INT); { Surface.Format.Inclusive e }
  | { Surface.Format.Open }

let exclusive ==
  | e = located(INT); { Surface.Format.Exclusive e }

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
