%token <int> INT
%token <string> NAME

%token KEWORD_DEF "def"
%token KEWORD_ELSE "else"
%token KEWORD_FORMAT "format"
%token KEWORD_IF "if"
%token KEWORD_LET "let"
%token KEWORD_THEN "then"
%token KEWORD_TYPE "type"

%token KEYWORD_FAIL "fail"
%token KEYWORD_PURE "pure"
%token KEYWORD_REPEAT_LEN "repeat-len"

%token BANG "!"
%token COLON ":"
%token COLON_EQUALS ":="
// %token COMMA ","
%token FULL_STOP "."
%token HYPHEN "-"
%token LESS_HYPHEN "<-"
%token SEMI ";"

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
  | "format"; n = located(NAME); ":="; tm = located(tm); ";";
      { Surface.FormatDef (n, tm) }
  | "format"; n = located(NAME); "{"; tms = trailing_list(";", record_format_field); "}";
      { Surface.RecordFormat (n, tms) }
  | "type"; n = located(NAME); ":="; tm = located(tm); ";";
      { Surface.TypeDef (n, tm) }
  | "type"; n = located(NAME); "{"; tms = trailing_list(";", ~ = located(NAME); ":"; ~ = located(tm); <>); "}";
      { Surface.RecordType (n, tms) }
  | "def"; n = located(NAME); tm1 = option(":"; ~ = located(tm); <>); ":="; tm2 = located(tm); ";";
      { Surface.TermDef (n, tm1, tm2) }

let record_format_field :=
  | "let"; n = located(NAME); ":="; tm1 = option(":"; ~ = located(tm); <>); tm2 = located(tm);
      { Surface.Let(n, tm1, tm2) }
  | "let"; n = located(NAME); "<-"; tm = located(tm);
      { Surface.Bind(n, tm) }
  | n = located(NAME); ":="; tm1 = option(":"; ~ = located(tm); <>); tm2 = located(tm);
      { Surface.LetField(n, tm1, tm2) }
  | n = located(NAME); "<-"; tm = located(tm);
      { Surface.BindField(n, tm) }

let tm :=
  | "let"; n = located(NAME); "<-"; tm1 = located(tm); ";"; tm2 = located(tm);
      { Surface.Bind(n, tm1, tm2) }
  | "let"; n = located(NAME); ":="; tm1 = option(":"; ~ = located(tm); <>); tm2 = located(tm); ";"; tm3 = located(tm);
      { Surface.Let(n, tm1, tm2, tm3) }
  | "if"; tm1 = located(tm); "then"; tm2 = located(tm); "else"; tm3 = located(tm);
      { Surface.IfThenElse(tm1, tm2, tm3) }
  // TODO: binary operators
  | tm1 = located(app_tm); ":"; tm2 = located(tm);
      { Surface.Ann (tm1, tm2) }
  | app_tm

let app_tm :=
  | "repeat-len"; tm1 = located(proj_tm); tm2 = located(proj_tm);
      { Surface.RepeatLen (tm1, tm2) }
  | "pure"; tm1 = located(proj_tm); tm2 = located(proj_tm);
      { Surface.Pure (tm1, tm2) }
  | "fail"; tm = located(proj_tm);
      { Surface.Fail tm }
  | proj_tm

let proj_tm :=
  | tm = located(proj_tm); "."; l = located(NAME);
      { Surface.Proj (tm, l) }
  | atomic_tm

let atomic_tm :=
  | "("; tm = tm; ")";
      { tm }
  | n = NAME;
      { Surface.Name n }
  | i = INT;
      { Surface.IntLit i }
  | "-"; tm = located(atomic_tm);
      { Surface.Op1(`Neg, tm) }
  | "!"; tm = located(atomic_tm);
      { Surface.Op1(`LogicalNot, tm) }
  | "{"; tms = trailing_list(";", ~ = located(NAME); ":="; ~ = located(tm); <>); "}";
      { Surface.RecordLit tms }

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
