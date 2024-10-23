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
%token COMMA ","
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
  | "format"; n = located(NAME); ":="; f = located(tm); ";";
      { Surface.FormatDef (n, f) }
  | "format"; n = located(NAME); "{"; fs = trailing_list(";", record_format_field); "}";
      { Surface.RecordFormat (n, fs) }
  | "type"; n = located(NAME); ":="; t = located(tm); ";";
      { Surface.TypeDef (n, t) }
  | "type"; n = located(NAME); "{"; fs = trailing_list(";", ~ = located(NAME); ":"; ~ = located(tm); <>); "}";
      { Surface.RecordType (n, fs) }
  | "def"; n = located(NAME); t = option(":"; ~ = located(tm); <>); ":="; e = located(tm); ";";
      { Surface.TermDef (n, t, e) }

let record_format_field :=
  | "let"; n = located(NAME); ":="; t = option(":"; ~ = located(tm); <>); e = located(tm);
      { Surface.Let(n, t, e) }
  | "let"; n = located(NAME); "<-"; f = located(tm);
      { Surface.Bind(n, f) }
  | n = located(NAME); ":="; t = option(":"; ~ = located(tm); <>); e = located(tm);
      { Surface.LetField(n, t, e) }
  | n = located(NAME); "<-"; f = located(tm);
      { Surface.BindField(n, f) }

let tm :=
  | "let"; n = located(NAME); "<-"; t1 = located(tm); ";"; t2 = located(tm);
      { Surface.Bind(n, t1, t2) }
  | "let"; n = located(NAME); ":="; t1 = option(":"; ~ = located(tm); <>); t2 = located(tm); ";"; t3 = located(tm);
      { Surface.Let(n, t1, t2, t3) }
  | "if"; t1 = located(tm); "then"; t2 = located(tm); "else"; t3 = located(tm);
      { Surface.IfThenElse(t1, t2, t3) }
  | tm1 = located(proj_tm); ":"; tm2 = located(tm);
      { Surface.Ann (tm1, tm2) }
  | proj_tm

let proj_tm :=
  | e = located(proj_tm); "."; l = located(NAME);
      { Surface.Proj (e, l) }
  | atomic_tm

let atomic_tm :=
  | "("; e = tm; ")";
      { e }
  | n = NAME;
      { Surface.Name n }
  | i = INT;
      { Surface.IntLit i }
  | "-"; f = located(atomic_tm);
      { Surface.Op1(`Neg, f) }
  | "!"; f = located(atomic_tm);
      { Surface.Op1(`LogicalNot, f) }
  | "{"; fs = trailing_list(";", ~ = located(NAME); ":="; ~ = located(tm); <>); "}";
      { Surface.RecordLit fs }
  | "repeat-len"; "("; e = located(tm); ","; f = located(tm); ")";
      { Surface.RepeatLen (e, f) }
  | "pure"; "("; t = located(tm); ","; e = located(tm); ")";
      { Surface.Pure (t, e) }
  | "fail"; "("; t = located(tm); ")";
      { Surface.Fail t }

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
