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

   type PValue is access Value;

   type PReference is access Reference;

   type BoolArray is array(Positive range <>) of aliased Boolean;

   type IntArray is array(Positive range <>) of aliased Integer;

   type FloatArray is array(Positive range <>) of aliased Float;

   type ReferencesArray is array(Positive range <>) of PReference;

   type BlockArguments(s : Positive) is record
      args: ReferencesArray(1 .. s);
   end record;

   type PBlockArguments is access all BlockArguments;

   type Block is access function(a: PBlockArguments) return Boolean;

   type ProgramInstructions is array (Positive range 1 .. 10000) of Block;

   function AddInt(a: PBlockArguments) return Boolean is
   begin
      a.args(3).all.i.all := a.args(2).all.i.all + a.args(1).all.i.all;
      return True;
   end AddInt;

   function SubInt(a: PBlockArguments) return Boolean is
   begin
      a.args(3).all.i.all := a.args(1).all.i.all - a.args(2).all.i.all;
      return True;
   end SubInt;

   function MulInt(a: PBlockArguments) return Boolean is
   begin
      a.args(3).all.i.all := a.args(1).all.i.all * a.args(2).all.i.all;
      return True;
   end MulInt;

   function DivInt(a: PBlockArguments) return Boolean is
   begin
      if a.args(2).all.i.all /= 0 then
         a.args(3).all.i.all := a.args(1).all.i.all / a.args(2).all.i.all;
      else
         a.args(3).all.i.all := Integer'Last;
      end if;
      return True;
   end DivInt;

   function ModInt(a: PBlockArguments) return Boolean is
   begin
      if a.args(2).all.i.all /= 0 then
         a.args(3).all.i.all := a.args(1).all.i.all mod a.args(2).all.i.all;
      else
         a.args(3).all.i.all := 0;
      end if;

      return True;
   end ModInt;

   function Stop(a: PBlockArguments) return Boolean is
   begin
      return False;
   end Stop;

   ibs : ProgramInstructions;

   -- memb : BoolArray(0 .. 511);
   memi : IntArray(1 .. 512);
   -- memf : FloatArray(0 .. 511);

   a : PBlockArguments;
   r : Boolean;
   j : Integer;
begin
   ibs(1) := AddInt'Access;
   ibs(2) := SubInt'Access;
   ibs(3) := MulInt'Access;
   ibs(4) := DivInt'Access;
   ibs(5) := ModInt'Access;
   ibs(6) := Stop'Access;

   a := new BlockArguments(3);
   a.all.args(1) := new Reference(TInt);
   a.all.args(1).all.i := memi(1)'Access;
   a.all.args(2) := new Reference(TInt);
   a.all.args(2).all.i := memi(2)'Access;
   a.all.args(3) := new Reference(TInt);
   a.all.args(3).all.i := memi(3)'Access;

   for i in 1 .. 10000000 loop
      j := i mod 6 + 1;
      if ibs(j) /= null then
         r := ibs(j)(a);
      end if;
   end loop;

end Main;
