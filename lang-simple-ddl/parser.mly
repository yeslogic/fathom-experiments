%token <string> INT
%token <string> NAME

%token KEYWORD_DEF "def"
%token KEYWORD_ELSE "else"
%token KEYWORD_FORMAT "format"
%token KEYWORD_IF "if"
%token KEYWORD_LET "let"
%token KEYWORD_THEN "then"
%token KEYWORD_TYPE "type"

%token AMPERSAND "&"
%token ASTERISK "*"
%token BANG "!"
%token BANG_EQUALS "!="
%token CARET "^"
%token COLON ":"
%token COLON_EQUALS ":="
%token EQUALS "="
%token GREATER ">"
%token GREATER_EQUAL ">="
%token GREATER_GREATER ">>"
// %token COMMA ","
%token FORWARD_SLASH "/"
%token FULL_STOP "."
%token HYPHEN "-"
%token LESS_HYPHEN "<-"
%token LESS "<"
%token LESS_EQUAL "<="
%token LESS_LESS "<<"
%token PIPE "|"
%token PLUS "+"
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
      { Surface.Format_def (n, tm) }
  | "format"; n = located(NAME); "{"; tms = trailing_list(";", record_format_field); "}";
      { Surface.Record_format (n, tms) }
  | "type"; n = located(NAME); ":="; tm = located(tm); ";";
      { Surface.Type_def (n, tm) }
  | "type"; n = located(NAME); "{"; tms = trailing_list(";", ~ = located(NAME); ":"; ~ = located(tm); <>); "}";
      { Surface.Record_type (n, tms) }
  | "def"; n = located(NAME); tm1 = option(":"; ~ = located(tm); <>); ":="; tm2 = located(tm); ";";
      { Surface.TermDef (n, tm1, tm2) }

let record_format_field :=
  | "let"; n = located(NAME); tm1 = option(":"; ~ = located(tm); <>); ":="; tm2 = located(tm);
      { Surface.Let(n, tm1, tm2) }
  | "let"; n = located(NAME); "<-"; tm = located(tm);
      { Surface.Bind(n, tm) }
  | n = located(NAME); tm1 = option(":"; ~ = located(tm); <>); ":="; tm2 = located(tm);
      { Surface.Let_field(n, tm1, tm2) }
  | n = located(NAME); "<-"; tm = located(tm);
      { Surface.Bind_field(n, tm) }

let tm :=
  | tm1 = located(let_tm); ":"; tm2 = located(tm);
      { Surface.Ann (tm1, tm2) }
  | let_tm

let let_tm :=
  | "let"; n = located(NAME); "<-"; tm1 = located(tm); ";"; tm2 = located(let_tm);
      { Surface.Bind(n, tm1, tm2) }
  | "let"; n = located(NAME); tm1 = option(":"; ~ = located(tm); <>); ":="; tm2 = located(tm); ";"; tm3 = located(let_tm);
      { Surface.Let(n, tm1, tm2, tm3) }
  | "if"; tm1 = located(tm); "then"; tm2 = located(let_tm); "else"; tm3 = located(let_tm);
      { Surface.If_then_else(tm1, tm2, tm3) }
  | cmp_tm

let cmp_tm :=
  | tm1 = located(or_tm); "="; tm2 = located(or_tm); { Surface.Op2 (`Eq, tm1, tm2) }
  | tm1 = located(or_tm); "!="; tm2 = located(or_tm); { Surface.Op2 (`Ne, tm1, tm2) }
  | tm1 = located(or_tm); "<="; tm2 = located(or_tm); { Surface.Op2 (`Le, tm1, tm2) }
  | tm1 = located(or_tm); "<"; tm2 = located(or_tm); { Surface.Op2 (`Lt, tm1, tm2) }
  | tm1 = located(or_tm); ">"; tm2 = located(or_tm); { Surface.Op2 (`Gt, tm1, tm2) }
  | tm1 = located(or_tm); ">="; tm2 = located(or_tm); { Surface.Op2 (`Ge, tm1, tm2) }
  | or_tm

let or_tm :=
  | tm1 = located(or_tm); "|"; tm2 = located(xor_tm); { Surface.Op2 (`Logical_or, tm1, tm2) }
  | xor_tm

let xor_tm :=
  | tm1 = located(xor_tm); "^"; tm2 = located(and_tm); { Surface.Op2 (`Logical_xor, tm1, tm2) }
  | and_tm

let and_tm :=
  | tm1 = located(and_tm); "&"; tm2 = located(shift_tm); { Surface.Op2 (`Logical_or, tm1, tm2) }
  | shift_tm

let shift_tm :=
  | tm1 = located(shift_tm); "<<"; tm2 = located(add_tm); { Surface.Op2 (`Logical_shl, tm1, tm2) }
  | tm1 = located(shift_tm); ">>"; tm2 = located(add_tm); { Surface.Op2 (`Arith_shr, tm1, tm2) }
  | add_tm

let add_tm :=
  | tm1 = located(mul_tm); "+"; tm2 = located(add_tm); { Surface.Op2 (`Add, tm1, tm2) }
  | tm1 = located(mul_tm); "-"; tm2 = located(add_tm); { Surface.Op2 (`Sub, tm1, tm2) }
  | mul_tm

let mul_tm :=
  | tm1 = located(app_tm); "*"; tm2 = located(mul_tm); { Surface.Op2 (`Mul, tm1, tm2) }
  | tm1 = located(app_tm); "/"; tm2 = located(mul_tm); { Surface.Op2 (`Div, tm1, tm2) }
  | app_tm

let app_tm :=
  | n = NAME; tms = nonempty_list(located(proj_tm));
      { Surface.Name(n, tms) }
  | "-"; tm = located(atomic_tm);
      { Surface.Op1(`Neg, tm) }
  | "!"; tm = located(atomic_tm);
      { Surface.Op1(`Logical_not, tm) }
  | proj_tm

let proj_tm :=
  | tm = located(proj_tm); "."; l = located(NAME);
      { Surface.Proj (tm, l) }
  | atomic_tm

let atomic_tm :=
  | "("; tm = tm; ")";
      { tm }
  | n = NAME;
      { Surface.Name (n, []) }
  | i = INT;
      { Surface.Int_lit i }
  | "{"; tms = trailing_list(";", ~ = located(NAME); ":="; ~ = located(tm); <>); "}";
      { Surface.Record_lit tms }

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
