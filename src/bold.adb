pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body bold is

   ------------
   -- AddInt --
   ------------

  function AddInt(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.i := a.all(2).all.i + a.all(1).all.i;
      return True;
   end AddInt;

   ------------
   -- SubInt --
   ------------

   function SubInt(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.i := a.all(2).all.i - a.all(1).all.i;
      return True;
   end SubInt;


   ------------
   -- MulInt --
   ------------

   function MulInt(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.i := a.all(2).all.i * a.all(1).all.i;
      return True;
--     exception
--        when Constraint_Error =>
--           a.all(3).all.i := Integer'Last;
--           return True;
   end MulInt;

   ------------
   -- DivInt --
   ------------

  function DivInt(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.i /= 0 then
         a.all(3).all.i := a.all(2).all.i / a.all(1).all.i;
      else
         a.all(3).all.i := Integer'Last;
      end if;
      return True;
   end DivInt;

   ------------
   -- ModInt --
   ------------

    function ModInt(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.i /= 0 then
         a.all(3).all.i := a.all(2).all.i mod a.all(1).all.i;
      else
         a.all(3).all.i := 0;
      end if;

      return True;
   end ModInt;

   -------------
   -- AndBool --
   -------------

   function AndBool(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.b := a.all(2).all.b and a.all(1).all.b;
      return True;
   end AndBool;

   ------------
   -- OrBool --
   ------------

   function OrBool(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.b := a.all(2).all.b or a.all(1).all.b;
      return True;
   end OrBool;

   -------------
   -- XorBool --
   -------------

   function XorBool(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.b := a.all(2).all.b xor a.all(1).all.b;
      return True;
   end XorBool;

   -------------
   -- NotBool --
   -------------

   function NotBool(a: PValuesArray) return Boolean is
   begin
      a.all(2).all.b := not a.all(1).all.b;
      return True;
   end NotBool;

   --------------
   -- AssignIf --
   --------------

   function AssignIf(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.b then
         a.all(3).all := a.all(2).all;
      end if;
      return True;
   end AssignIf;

   -------------
   -- Select2 --
   -------------

  function Select2(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.b then
         a.all(4).all := a.all(2).all;
      else
         a.all(4).all := a.all(3).all;
      end if;
      return True;
   end Select2;

   ----------
   -- Stop --
   ----------

  function Stop(a: PValuesArray) return Boolean is
   begin
      return False;
   end Stop;

   ------------
   -- DoTest --
   ------------

   procedure DoTest is
      ibs : ProgramInstructions;

      a : ValuesArray(1 ..3);
      b : ValuesArray(1 ..3);
      mem : ValuesArray(1 .. 1024);
      pmem : PValuesArray;
      r : Boolean;
      j : Integer;
   begin
      pmem := new ValuesArray(1 .. 1024);

      pmem(1) := new Value(TInt);
      pmem(2) := new Value(TInt);
      pmem(3) := new Value(TInt);
      pmem(4) := new Value(TInt);
      pmem(5) := new Value(TInt);
      pmem(6) := new Value(TInt);
      pmem(7) := new Value(TInt);
      pmem(8) := new Value(TInt);

      pmem(9) := new Value(TBool);
      pmem(10) := new Value(TBool);
      pmem(11) := new Value(TBool);
      pmem(12) := new Value(TBool);
      pmem(13) := new Value(TBool);
      pmem(14) := new Value(TBool);

      ibs(1) := (AddInt'Access, new ValuesArray(1 .. 3));
      ibs(1).args.all := (pmem(1), pmem(2), pmem(1));
      pmem(1).all.i := 0;
      pmem(2).all.i := 1;

      ibs(2) := (SubInt'Access, new ValuesArray(1 .. 3));
      ibs(2).args.all := (pmem(1), pmem(3), pmem(4));
      pmem(3).all.i := 1;

      ibs(3) := (MulInt'Access, new ValuesArray(1 .. 3));
      ibs(3).args.all := (pmem(5), pmem(7), pmem(6));
      pmem(7).all.i := 1;
      -- NOTE: ^ this is workaround for the raised Exception(ValueOverflow)

      ibs(4) := (NotBool'Access, new ValuesArray(1 .. 2));
      ibs(4).args.all := (pmem(9), pmem(10));

      ibs(5) := (XorBool'Access, new ValuesArray(1 .. 3));
      ibs(5).args.all := (pmem(10), pmem(11), pmem(12));

      ibs(6) := (OrBool'Access, new ValuesArray(1 .. 3));
      ibs(6).args.all := (pmem(12), pmem(13), pmem(14));

      ibs(7) := (AssignIf'Access, new ValuesArray(1 .. 3));
      ibs(7).args.all := (pmem(12), pmem(4), pmem(6));

      for i in 1 .. 100_000_000 loop
         j := i mod 7 + 1;
         if ibs(j).func /= null then
            r := ibs(j).func(ibs(j).args);
         end if;
      end loop;

      Put_Line(pmem(1).all.i'Image);
   end DoTest;

end bold;
