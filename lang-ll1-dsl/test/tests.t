Elaborate comments
  $ cat comments.txt | ll1-dsl

Elaborate base
  $ cat base.txt | ll1-dsl
  def fail : Format := {};
  
  def unit : Format := ();
  
  def any-byte : Format := {0..255};
  

Elaborate ranges
  $ cat ranges.txt | ll1-dsl
  def singletons : Format := {0} | {23};
  
  def neg : Format := {0..22, 24..255};
  
  def full : Format := {0..255};
  
  def inclusive-open : Format := {128..255} | {0..127};
  
  def exclusive-open : Format := {129..255} | {0..127};
  
  def exclusive-closed : Format := {129..254};
  

Elaborate ASCII
  $ cat ascii.txt | ll1-dsl
  def ascii-null : Format := {0};
  
  def ascii-printable : Format := {32..126};
  
  def ascii-number : Format := {48..57};
  
  def ascii-letter : Format := {65..90} | {97..122};
  
  def test-items : Format := ascii-null | ascii-number | ascii-letter;
  
Elaborate actions
  $ cat actions.txt | ll1-dsl
  def test-actions : Format := map @(Pair Byte Byte) (x => x, x) {0};
  
  def test-actions : Format :=
    map @Unit (x => ()) {0} | map @Unit (y => ()) {1};
  
