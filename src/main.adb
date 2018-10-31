with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   type DataType is (TBool, TInt, TFloat);

   type Value (t: DataType) is record
      case t is
         when TBool   => b : Boolean;
         when TInt    => i : Integer;
         when TFloat  => f : Float;
      end case;
   end record;

   type PValue is access all Value;

   type BoolArray is array(Positive range <>) of aliased Boolean;

   type IntArray is array(Positive range <>) of aliased Integer;

   type FloatArray is array(Positive range <>) of aliased Float;

   type ValuesArray is array(Positive range <>) of PValue;

   type PValuesArray is access all ValuesArray;


   type BlockBody is access function(a: PValuesArray) return Boolean;

   type Block is record
      func : BlockBody;
      args : PValuesArray;
   end record;

   type ProgramInstructions is array (Positive range 1 .. 10000) of Block;

   function AddInt(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.i := a.all(2).all.i + a.all(1).all.i;
      return True;
   end AddInt;

   function SubInt(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.i := a.all(2).all.i - a.all(1).all.i;
      return True;
   end SubInt;

   function MulInt(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.i := a.all(2).all.i * a.all(1).all.i;
      return True;
--     exception
--        when Constraint_Error =>
--           a.all(3).all.i := Integer'Last;
--           return True;
   end MulInt;

   function DivInt(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.i /= 0 then
         a.all(3).all.i := a.all(2).all.i / a.all(1).all.i;
      else
         a.all(3).all.i := Integer'Last;
      end if;
      return True;
   end DivInt;

   function ModInt(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.i /= 0 then
         a.all(3).all.i := a.all(2).all.i mod a.all(1).all.i;
      else
         a.all(3).all.i := 0;
      end if;

      return True;
   end ModInt;

   function AndBool(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.b := a.all(2).all.b and a.all(1).all.b;
      return True;
   end AndBool;

   function OrBool(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.b := a.all(2).all.b or a.all(1).all.b;
      return True;
   end OrBool;

   function XorBool(a: PValuesArray) return Boolean is
   begin
      a.all(3).all.b := a.all(2).all.b xor a.all(1).all.b;
      return True;
   end XorBool;

   function NotBool(a: PValuesArray) return Boolean is
   begin
      a.all(2).all.b := not a.all(1).all.b;
      return True;
   end NotBool;

   function AssignIf(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.b then
         a.all(3).all := a.all(2).all;
      end if;
      return True;
   end AssignIf;

   function Select2(a: PValuesArray) return Boolean is
   begin
      if a.all(1).all.b then
         a.all(4).all := a.all(2).all;
      else
         a.all(4).all := a.all(3).all;
      end if;
      return True;
   end Select2;

   function Stop(a: PValuesArray) return Boolean is
   begin
      return False;
   end Stop;

   ibs : ProgramInstructions;

   -- memb : BoolArray(0 .. 511);
   -- memi : IntArray(1 .. 512);
   -- memf : FloatArray(0 .. 511);

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
end Main;

-- elapsed time: 00.75s (mode:default; 1e7 instr)
-- elapsed time: 00.44s (mode:optimize; 1e7 instr)
-- elapsed time: 01.00s (mode:optimize; 1e8 instr)
-- elapsed time: 03.10s (mode:default; 1e8 instr)

-- this results may be good, but may improved
-- if a descriminator check will be avoided
