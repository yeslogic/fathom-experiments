Elaborate comments
  $ cat comments.txt | ll1-dsl

Elaborate base
  $ cat base.txt | ll1-dsl
  def fail : Format := {};
  
  def unit : Format := ();
  
  def any-byte : Format := {0..255};
  

Elaborate ASCII
  $ cat ascii.txt | ll1-dsl
  def ranges : Format :=
    {0} | {23} | {0..22, 25..255} | {0..255} | {128..255} | {0..255} |
    {129..255} | {0..255} | {129..254};
  
  def ascii-null : Format := {0};
  
  def ascii-printable : Format := {32..126};
  
  def ascii-number : Format := {48..57};
  
  def ascii-letter : Format := {65..90} | {97..122};
  
  def test-items : Format :=
    ascii-null | ascii-printable | ascii-number | ascii-letter;
  
