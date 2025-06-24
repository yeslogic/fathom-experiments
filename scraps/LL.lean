/-!
  # Binary decoder DSL
-/

inductive Either α β where
| left : α → Either α β
| right : β → Either α β

/-! ## Grammars -/

abbrev Decode (α : Type u) :=
  (bytes : ByteArray) -> Nat -> Option (Nat × α)


structure Grammar where
  type : Type u
  decode : Decode type

/--
  A binary format constrained to a specific type
-/
abbrev GrammarOf (α : Type u) :=
  { g : Grammar // g.type = α }

-- TODO: Figure out how to make this a coercion
def GrammarOf.mk (g : Grammar) : GrammarOf g.type :=
  ⟨g, rfl⟩

def GrammarOf.type (_ : GrammarOf α) : Type u := α

def GrammarOf.decode (g : GrammarOf α) : Decode α :=
  fun bytes i =>
    (g.val.decode bytes i).map (fun ⟨i, v⟩ => ⟨i, cast g.property v⟩)


/-! ## Combinators -/

def unit : Grammar where
  type := Unit
  decode _ i := pure ⟨i, ()⟩

def empty : Grammar where
  type := Empty
  decode _ _ := none

def pure (v : α) : Grammar where
  type := α
  decode _ i := Pure.pure ⟨i, v⟩

def map (g : Grammar) (f : g.type → α) : Grammar where
  type := α
  decode bytes i :=
    (g.decode bytes i).map (fun ⟨i, v⟩ => ⟨i, f v⟩)

def byte : Grammar where
  type := UInt8
  decode bytes i :=
    match Nat.decLt i bytes.size with
    | isFalse _ => none
    | isTrue _ => pure ⟨i + 1, bytes.get i⟩

def alt (g₁ : GrammarOf α) (g₂ : GrammarOf α) : Grammar where
  type := α
  decode bytes i :=
    g₁.decode bytes i <|> g₂.decode bytes i

def either (g₁ g₂ : Grammar) : Grammar :=
  alt (.mk (map g₁ Either.left)) (.mk (map g₂ Either.right))

variable {x y α : Type u} in
def branch (g : GrammarOf (Either x y)) (f₁ : x → α) (f₂ : y → α) : Grammar where
  type := α
  decode bytes i := do
    match ← g.decode bytes i with
    | ⟨i, .left x⟩ => pure ⟨i, f₁ x⟩
    | ⟨i, .right y⟩ => pure ⟨i, f₂ y⟩

def pair (g₁ g₂ : Grammar) : Grammar where
  type := g₁.type × g₂.type
  decode bytes i := do
    let ⟨i, v₁⟩ ← g₁.decode bytes i
    let ⟨i, v₂⟩ ← g₂.decode bytes i
    pure ⟨i, ⟨v₁, v₂⟩⟩

def sig (g₁ : Grammar) (g₂ : g₁.type → Grammar) : Grammar where
  type := Σ (x : g₁.type), (g₂ x).type
  decode bytes i := do
    let ⟨i, v₁⟩ ← g₁.decode bytes i
    let ⟨i, v₂⟩ ← (g₂ v₁).decode bytes i
    pure ⟨i, ⟨v₁, v₂⟩⟩

def repeatCount (n : Nat) (g : Grammar) : Grammar where
  type := Vector g.type n
  decode := decodeVector #v[] where
    decodeVector : {n m : Nat} → Vector g.type m → Decode (Vector g.type (n + m))
      | 0, _, xs, _, i =>
          by rw [Nat.zero_add]; exact some ⟨i, xs⟩
      | n + 1, _, xs, bytes, i => do
          let ⟨i, v⟩ ← g.decode bytes i
          by rw [Nat.add_right_comm]; exact decodeVector (xs.push v) bytes i


/-! ## Examples -/

example : Grammar :=
  sig (pair byte byte) fun ⟨width, height⟩ =>
    repeatCount (width.toFin * height.toFin) byte


/-! ## Grammar DSL -/

declare_syntax_cat                      grammar
syntax "u8"                           : grammar
syntax grammar "|" grammar            : grammar
syntax grammar ";" grammar            : grammar
syntax "repeat-count" term grammar    : grammar
syntax "{" (ident "←" grammar),* "}"  : grammar
syntax term                           : grammar
syntax "[Grammar|" grammar "]"        : term

macro_rules
  | `([Grammar| u8]) => `(byte)
  | `([Grammar| $g₁ | $g₂]) => `(alt [Grammar| $g₁] [Grammar| $g₂])
  | `([Grammar| $g₁ ; $g₂]) => `(pair [Grammar| $g₁] [Grammar| $g₂])
  | `([Grammar| repeat-count $n:term $g:grammar]) => `(repeatCount $n [Grammar| $g])
  -- | `([Grammar| { $gs,* }]) => sorry
  | `([Grammar| $t:term]) => `($t)

example : Grammar := [Grammar| u8]
example : Grammar := [Grammar| u8; u8; u8]
example : Grammar := [Grammar| repeat-count 3 u8]

-- example : Grammar :=
--   [Grammar| {
--     width ← u8;
--     height ← u8;
--     data ← repeat-count (width * height) u8
--   }]
