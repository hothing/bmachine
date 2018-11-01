package bnew is

   
   type DataType is (TBool, TInt, TFloat);
   
   type PBool is access all Boolean;
   
   type PInt is access all Integer;
   
   type PFloat is access all Float;
   
   type RAny (t: DataType := TInt) is record
      case t is
         when TBool   => pb : PBool;
         when TInt    => pi : PInt;
         when TFloat  => pf : PFloat;
      end case;
   end record;
   pragma Unchecked_Union(RAny);
   
   type PAny is access all RAny;
   
   type Block is abstract tagged null record;
   
   type PBlock is access all Block'Class;
   
   type Instructions is array (Positive range <>) of PBlock;
   
   procedure DoCalc(b : Block);
   procedure Calculate(b : Block'Class);   
   
   type BlockAddI is new Block with record
      i1 : RAny(TInt);
      i2 : RAny(TInt);
      q1 : RAny(TInt);
   end record;
   
   type PBlockAddI is access BlockAddI;
   
   procedure DoCalc(b : BlockAddI);
   procedure Init (b : in out BlockAddI; i1, i2: PInt; q1 : PInt);
   
   type BlockSubI is new Block with record
      i1 : RAny(TInt);
      i2 : RAny(TInt);
      q1 : RAny(TInt);
   end record;
   
   type PBlockSubI is access BlockSubI;
   
   procedure DoCalc(b : BlockSubI);
   procedure Init (b : in out BlockSubI; i1, i2: PInt; q1 : PInt);
   
   procedure DoTest;   
   
end bnew;
