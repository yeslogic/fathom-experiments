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
  
  def neg : Format := {0..22, 25..255};
  
  def full : Format := {0..255};
  
  def inclusive_open : Format := {128..255} | {0..127};
  
  def exclusive_open : Format := {129..255} | {0..127};
  
  def exclusive_closed : Format := {129..254};
  

Elaborate ASCII
  $ cat ascii.txt | ll1-dsl
  def ascii-null : Format := {0};
  
  def ascii-printable : Format := {32..126};
  
  def ascii-number : Format := {48..57};
  
  def ascii-letter : Format := {65..90} | {97..122};
  
  def test-items : Format := ascii-null | ascii-number | ascii-letter;
  
