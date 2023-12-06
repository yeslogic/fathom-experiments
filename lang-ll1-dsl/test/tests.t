Elaborate comments
  $ cat comments.txt | ll1-dsl

Elaborate items
  $ cat items.txt | ll1-dsl
  def no-ann : Format := pure @Unit ();
  
  def format-ann : Format := pure @Unit ();
  
  def Byte : Type := Byte;
  
  def Pair : Type := Pair Byte Byte;
  
  def Unit : Type := Unit;
  
  def test-repr : Type := Unit;
  
  def foo : Byte := 42;
  
  def bar : Byte := 42;
  

Elaborate base
  $ cat base.txt | ll1-dsl
  def fail : Format := {};
  
  def unit : Format := pure @Unit ();
  
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
  
Elaborate JPEG
  $ cat jpeg.txt | ll1-dsl
  def sof0 : Format := {255}, {192};
  
  def sof1 : Format := {255}, {193};
  
  def sof2 : Format := {255}, {194};
  
  def sof3 : Format := {255}, {195};
  
  def dht : Format := {255}, {196};
  
  def sof5 : Format := {255}, {197};
  
  def sof6 : Format := {255}, {198};
  
  def sof7 : Format := {255}, {199};
  
  def jpeg : Format := {255}, {200};
  
  def sof9 : Format := {255}, {201};
  
  def sof10 : Format := {255}, {202};
  
  def sof11 : Format := {255}, {203};
  
  def dac : Format := {255}, {204};
  
  def sof13 : Format := {255}, {205};
  
  def sof14 : Format := {255}, {206};
  
  def sof15 : Format := {255}, {207};
  
  def rst0 : Format := {255}, {208};
  
  def rst1 : Format := {255}, {209};
  
  def rst2 : Format := {255}, {210};
  
  def rst3 : Format := {255}, {211};
  
  def rst4 : Format := {255}, {212};
  
  def rst5 : Format := {255}, {213};
  
  def rst6 : Format := {255}, {214};
  
  def rst7 : Format := {255}, {215};
  
  def soi : Format := {255}, {216};
  
  def eoi : Format := {255}, {217};
  
  def sos : Format := {255}, {218};
  
  def dqt : Format := {255}, {219};
  
  def dnl : Format := {255}, {220};
  
  def dri : Format := {255}, {221};
  
  def dhp : Format := {255}, {222};
  
  def exp : Format := {255}, {223};
  
  def app0 : Format := {255}, {224};
  
  def app1 : Format := {255}, {225};
  
  def app2 : Format := {255}, {226};
  
  def app3 : Format := {255}, {227};
  
  def app4 : Format := {255}, {228};
  
  def app5 : Format := {255}, {229};
  
  def app6 : Format := {255}, {230};
  
  def app7 : Format := {255}, {231};
  
  def app8 : Format := {255}, {232};
  
  def app9 : Format := {255}, {233};
  
  def app10 : Format := {255}, {234};
  
  def app11 : Format := {255}, {235};
  
  def app12 : Format := {255}, {236};
  
  def app13 : Format := {255}, {237};
  
  def app14 : Format := {255}, {238};
  
  def app15 : Format := {255}, {239};
  
  def com : Format := {255}, {254};
  

Elaborate actions
  $ cat actions.txt | ll1-dsl
  def test-actions : Format := map @(Pair Byte Byte) (x => x, x) {0};
  
  def test-actions : Format :=
    map @Unit (x => ()) {0} | map @Unit (y => ()) {1};
  
