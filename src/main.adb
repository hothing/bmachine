procedure Main is

   type PBool is access all Boolean;
   type PInt is access all Integer;
   type PFloat is access all Float;

   type DataType is (TBool, TInt, TFloat);
   type Reference (t: DataType) is record
      case t is
         when TBool   => b : PBool;
         when TInt    => i : PInt;
         when TFloat  => f : PFloat;
      end case;
   end record;

   type Value (t: DataType) is record
      case t is
         when TBool   => b : Boolean;
         when TInt    => i : Integer;
         when TFloat  => f : Float;
      end case;
   end record;

   type PValue is access all Value;

   type PReference is access Reference;

   type BoolArray is array(Positive range <>) of aliased Boolean;

   type IntArray is array(Positive range <>) of aliased Integer;

   type FloatArray is array(Positive range <>) of aliased Float;

   type ReferencesArray is array(Positive range <>) of PReference;

   type ValuesArray is array(Positive range <>) of PValue;

   type PValuesArray is access all ValuesArray;

   type ArgumentsMap is array(Positive range <>) of Positive;

   type BlockArguments(s: Positive; m : PValuesArray) is record
      args: ArgumentsMap(1 .. s);
   end record;

   type PBlockArguments is access all BlockArguments;

   type Block is access function(a: ValuesArray) return Boolean;

   type ProgramInstructions is array (Positive range 1 .. 10000) of Block;

   function AddInt(a: ValuesArray) return Boolean is
   begin
      a(3).all.i := a(2).all.i + a(1).all.i;
      return True;
   end AddInt;

   function SubInt(a: ValuesArray) return Boolean is
   begin
      a(3).all.i := a(2).all.i - a(1).all.i;
      return True;
   end SubInt;

   function MulInt(a: ValuesArray) return Boolean is
   begin
      a(3).all.i := a(2).all.i * a(1).all.i;
      return True;
   exception
      when Constraint_Error =>
         a(3).all.i := Integer'Last;
         return True;
   end MulInt;

   function DivInt(a: ValuesArray) return Boolean is
   begin
      if a(1).all.i /= 0 then
         a(3).all.i := a(2).all.i / a(1).all.i;
      else
         a(3).all.i := Integer'Last;
      end if;
      return True;
   end DivInt;

   function ModInt(a: ValuesArray) return Boolean is
   begin
      if a(1).all.i /= 0 then
         a(3).all.i := a(2).all.i mod a(1).all.i;
      else
         a(3).all.i := 0;
      end if;

      return True;
   end ModInt;

   function Stop(a: ValuesArray) return Boolean is
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
   ibs(1) := AddInt'Access;
   ibs(2) := SubInt'Access;
   ibs(3) := AddInt'Access;
   ibs(4) := DivInt'Access;
   ibs(5) := ModInt'Access;
   ibs(6) := Stop'Access;

   pmem := new ValuesArray(1 .. 1024);

   pmem(1) := new Value(TInt);
   pmem(2) := new Value(TInt);
   pmem(3) := new Value(TInt);
   pmem(4) := new Value(TInt);
   pmem(5) := new Value(TInt);
   pmem(6) := new Value(TInt);

   a := (pmem(1), pmem(2), pmem(3));
   b := (pmem(3), pmem(4), pmem(5));

   for i in 1 .. 10000000 loop
      j := i mod 6 + 1;
      if ibs(j) /= null then
         r := ibs(j)(a);
      end if;
   end loop;

end Main;
