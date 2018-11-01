package bold is

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
   
   --------------------------------
   

   function AddInt(a: PValuesArray) return Boolean;

   function SubInt(a: PValuesArray) return Boolean;

   function MulInt(a: PValuesArray) return Boolean;

   function DivInt(a: PValuesArray) return Boolean;

   function ModInt(a: PValuesArray) return Boolean;
   
   function AndBool(a: PValuesArray) return Boolean;

   function OrBool(a: PValuesArray) return Boolean;

   function XorBool(a: PValuesArray) return Boolean;

   function NotBool(a: PValuesArray) return Boolean;

   function AssignIf(a: PValuesArray) return Boolean;
   
   function Select2(a: PValuesArray) return Boolean;

   function Stop(a: PValuesArray) return Boolean;
   
   procedure DoTest;
   

end bold;
