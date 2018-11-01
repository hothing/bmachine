pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;

package body bnew is


   procedure DoCalc (b : Block) is
   begin
     null;
   end DoCalc;

   ---------------
   -- Calculate --
   ---------------

   procedure Calculate (b : Block'Class) is
   begin
      DoCalc(b);
   end Calculate;

   ------------
   -- DoCalc --
   ------------

   procedure DoCalc (b : BlockAddI) is
   begin
     b.q1.pi.all := b.i1.pi.all + b.i2.pi.all;
   end DoCalc;

   procedure Init (b : in out BlockAddI; i1, i2: PInt; q1 : PInt) is
   begin
      b.i1.pi := i1;
      b.i2.pi := i2;
      b.q1.pi := q1;
   end Init;

   procedure DoCalc (b : BlockSubI) is
   begin
     b.q1.pi.all := b.i1.pi.all - b.i2.pi.all;
   end DoCalc;

   procedure Init (b : in out BlockSubI; i1, i2: PInt; q1 : PInt) is
   begin
      b.i1.pi := i1;
      b.i2.pi := i2;
      b.q1.pi := q1;
   end Init;
   --------------------------

   procedure DoTest is
      a : PInt;
      b : Pint;
      c : PInt;
      d : PInt;

      bz : PBlock;
      b1 : PBlockAddI;
      b2 : PBlockSubI;

      ibs : Instructions(1 .. 2);

      j : Integer;

   begin
      a := new Integer'(1);
      b := new Integer'(2);
      c := new Integer;
      d := new Integer'(3);

      -- ibs(1) := new BlockAddI (i1.pi => a, i2.pi => b, q1.pi => c);
      -- ibs(2) := new BlockSubI (i1.pi => c, i2.pi => d, q1.pi => c);

      b1 := new BlockAddI;
      b2 := new BlockSubI;

      Init(b1.all, a, b ,c);
      Init(b2.all, c, d ,c);

      ibs(1) := PBlock(b1);
      ibs(2) := PBlock(b2);

      j := ibs'First;
      for i in 1 .. 100_000_000 loop
         -- j := i mod ibs'Last + 1;
         bz := ibs(j);
         Calculate(bz.all);
         j := j + 1; if j > ibs'Last then j := ibs'First; end if;
      end loop;

      Put_Line(c.all'Image);

   end DoTest;

end bnew;
