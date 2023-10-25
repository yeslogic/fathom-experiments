# Core language

Syntax:

```text
t ::= ...   types
e ::= ...   expressions
f ::= ...   format descriptions
p ::= ...   programs
```

Contexts:

```text
S ::=
  | ∅
  | i : Format, S
```

```text
L ::=
  | ∅
  | x : t, L
```

Forms of judgement:

```text
i : Format ∈ S                A reference to a top-level item.
x : t      ∈ L                A reference to a local binding.

S    ⊢ program(p)             Well-formed programs.
S    ⊢ format(f)              Well-formed formats.
S    ⊢ type(t)                Well-formed types.
S; L ⊢ synth(e) ⇒ t           Synthesise the type of an expression.
S; L ⊢ check(e) ⇐ t           Check an expression against a type annotation.
```

## Programs

```text
p ::=
  | ∅
  | def x : Format := f; p
```

```text
──────────────────
  S ⊢ program(∅)
```

```text
  S ⊢ format(f)
  i : Format, S ⊢ program(p)
───────────────────────────────────────
  S ⊢ program(def i : Format := f; p)
```

## Formats

Syntax:

```text
f ::= ...
  | i
  | ()
  | bs      where bs ⊂ 0..255
  | f, f
  | f | f
  | map @t (x => e) f
```

Introduction:

```text
───────────────────
  S ⊢ format(())
```

```text
  i : Format ∈ S
──────────────────
  S ⊢ format(i)
```

```text
──────────────────
  S ⊢ format(bs)
```

```text
  S ⊢ format(f₀)
  S ⊢ format(f₁)
  S ⊢ separate(f₀, f₁)
────────────────────────
  S ⊢ format(f₀, f₁)
```

```text
  S ⊢ format(f₀)
  S ⊢ format(f₁)
  S ⊢ non-overlapping(f₀, f₁)
  repr(f₀) ≡ repr(f₁)
───────────────────────────────
  S ⊢ format(f₀ | f₁)
```

```text
  S ⊢ format(f)
  S; x : repr(f), L ⊢ synth-ty(e) ⇒ t
───────────────────────────────────────
  S ⊢ format(map @t (x => e) f)
```

## Structural

Syntax:

```text
e ::= ...
  | x
  | e : t
```

Local variables:

```text
  x : t ∈ L
───────────────────────
  S; L ⊢ synth(x) ⇒ t
```

Conversion:

```text
  S; L ⊢ synth(e) ⇒ t₁
  t₀ ≡ t₁
────────────────────────
  S; L ⊢ check(e) ⇐ t₀
```

Annotation:

```text
  S; L ⊢ check(e) ⇐ t
───────────────────────────
  S; L ⊢ synth(e : t) ⇒ t
```

## Unit

Syntax:

```text
t ::= ...
  | Unit

e ::= ...
  | ()
```

Formation:

```text
──────────────────
  S ⊢ type(Unit)
```

Introduction:

```text
───────────────────────────
  S; L ⊢ synth(()) ⇒ Unit
```

## Bytes

Syntax:

```text
t ::= ...
  | Byte

e ::= ...
  | b       where b ∈ 0..255
```

Formation:

```text
──────────────────
  S ⊢ type(Byte)
```

Introduction:

```text
──────────────────────────
  S; L ⊢ synth(b) ⇒ Byte
```

## Pairs

Syntax:

```text
t ::= ...
  | Pair t t

e ::= ...
  | e, e
```

Formation:

```text
  S ⊢ type(t₀)
  S ⊢ type(t₁)
────────────────────────
  S ⊢ type(Pair t₀ t₁)
```

Introduction:

```text
  S; L ⊢ synth(e₀) ⇒ t₀
  S; L ⊢ synth(e₁) ⇒ t₁
────────────────────────────────────
  S; L ⊢ synth(e₀, e₁) ⇒ Pair t₀ t₁
```
