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

   procedure DoCalc (b : BlockMulI) is
   begin
     b.q1.pi.all := b.i1.pi.all * b.i2.pi.all;
   end DoCalc;

   procedure DoCalc (b : BlockDivI) is
   begin
     b.q1.pi.all := b.i1.pi.all / b.i2.pi.all;
   end DoCalc;


   --------------------------

   package body Constructors is

      function NewAddI (i1, i2: PInt; q1 : PInt) return PBlock is
         b : PBlockAddI;
      begin
         b := new BlockAddI;
         Init(b.all, i1, i2,q1);
         return PBlock(b);
      end NewAddI;


      function NewSubI (i1, i2: PInt; q1 : PInt) return PBlock is
         b : PBlockSubI;
      begin
         b := new BlockSubI;
         Init(b.all, i1, i2,q1);
         return PBlock(b);
      end NewSubI;

      function NewMulI (i1, i2: PInt; q1 : PInt) return PBlock is
         b : PBlockMulI;
      begin
         b := new BlockMulI;
         Init(b.all, i1, i2,q1);
         return PBlock(b);
      end NewMulI;

      function NewDivI (i1, i2: PInt; q1 : PInt) return PBlock is
         b : PBlockDivI;
      begin
         b := new BlockDivI;
         Init(b.all, i1, i2,q1);
         return PBlock(b);
      end NewDivI;

   end Constructors;


   --------------------------

   procedure DoTest is

      mi : PMemInt := new MemInt(1 .. 10);

      bz : PBlock;

      ibs : Instructions(1 .. 4);

      j : Integer;

   begin

      mi(1) := 1;
      mi(2) := 2;
      mi(3) := 3;
      mi(5) := 10; -- Integer'Last;
      mi(6) := 5; -- Integer'First;
      mi(8) := 10;

      ibs(1) := Constructors.NewAddI(mi(1)'Access, mi(2)'Access, mi(4)'Access);
      ibs(2) := Constructors.NewSubI(mi(4)'Access, mi(3)'Access, mi(4)'Access);
      ibs(3) := Constructors.NewMulI(mi(5)'Access, mi(6)'Access, mi(7)'Access);
      ibs(4) := Constructors.NewDivI(mi(7)'Access, mi(8)'Access, mi(9)'Access);

      j := ibs'First;
      for i in 1 .. 100_000_000 loop
         -- j := i mod ibs'Last + 1;
         bz := ibs(j);
         Calculate(bz.all);
         j := j + 1; if j > ibs'Last then j := ibs'First; end if;
      end loop;

      Put_Line(mi(4)'Image);

   end DoTest;

end bnew;
