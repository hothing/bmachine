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

   ibs(1) := (AddInt'Access, new ValuesArray(1 .. 3));
   ibs(1).args.all := (pmem(1), pmem(2), pmem(3));

   ibs(2) := (SubInt'Access, new ValuesArray(1 .. 3));
   ibs(2).args.all := (pmem(3), pmem(4), pmem(5));

   ibs(3) := (MulInt'Access, new ValuesArray(1 .. 3));
   ibs(3).args.all := (pmem(5), pmem(7), pmem(6));
   pmem(7).all.i := 1;
   -- NOTE: ^ this is workaround for the raised Exception(ValueOverflow)

   for i in 1 .. 10000000 loop
      j := i mod 3 + 1;
      if ibs(j).func /= null then
         r := ibs(j).func(ibs(j).args);
      end if;
   end loop;

end Main;

-- elapsed time: 00.75s (mode:default; 1e7 instr)
-- elapsed time: 00.44s (mode:optimize; 1e7 instr)
