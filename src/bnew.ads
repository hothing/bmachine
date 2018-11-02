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
   
   type MemBool is array(Positive range <>) of aliased Boolean;
   type PMemBool is access MemBool;
   
   type MemInt is array(Positive range <>) of aliased Integer;
   type PMemInt is access MemInt;
   
   type MemFloat is array(Positive range <>) of aliased Float;
   type PMemFloat is access MemFloat;
   
   ---------------------------------------------------------------------------
   
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
   
   type BlockSubI is new BlockAddI with null record;
   
   type PBlockSubI is access BlockSubI;
   
   procedure DoCalc(b : BlockSubI);
   -- procedure Init (b : in out BlockSubI; i1, i2: PInt; q1 : PInt);
   
   
   type BlockMulI is new BlockAddI with null record;
   
   type PBlockMulI is access BlockMulI;
   
   procedure DoCalc(b : BlockMulI);
   -- procedure Init (b : in out BlockMulI; i1, i2: PInt; q1 : PInt);
   
   
   type BlockDivI is new BlockAddI with null record;
   
   type PBlockDivI is access BlockDivI;
   
   procedure DoCalc(b : BlockDivI);
   -- procedure Init (b : in out BlockDivI; i1, i2: PInt; q1 : PInt);
   
   -----------------
   
   type BlockNot is new Block with record
      i1 : RAny(TBool);
      q1 : RAny(TBool);
   end record;
   
   procedure DoCalc(b : BlockNot);
   procedure Init (b : in out BlockNot; i1: PBool; q1 : PBool);
   
   type PBlockNot is access BlockNot;
   
   type BlockAnd is new BlockNot with record
      i2 : RAny(TBool);
   end record;
   
   procedure DoCalc(b : BlockAnd);
   procedure Init (b : in out BlockAnd; i1, i2: PBool; q1 : PBool);
   
   type PBlockAnd is access BlockAnd;
   
   type BlockOr is new BlockAnd with null record;
   
   procedure DoCalc(b : BlockOr);
   
   type PBlockOr is access BlockOr;
   
   type BlockXor is new BlockAnd with null record;
   
   procedure DoCalc(b : BlockXor);
   
   type PBlockXor is access BlockXor;
   
   type BlockJump is new Block with record
      i1 : RAny(TBool);
      q1 : RAny(TInt);
      offset : Integer;
   end record;
   
   type PBlockJump is access BlockJump;
   
   procedure DoCalc(b : BlockJump);
   
   -----------------
   
   package Constructors is
      
      function NewAddI (i1, i2: PInt; q1 : PInt) return PBlock;
      
      function NewSubI (i1, i2: PInt; q1 : PInt) return PBlock;
      
      function NewMulI (i1, i2: PInt; q1 : PInt) return PBlock;
      
      function NewDivI (i1, i2: PInt; q1 : PInt) return PBlock;
      
      
      function NewNot (i1 : PBool; q1 : PBool) return PBlock;
      
      function NewAnd (i1, i2: PBool; q1 : PBool) return PBlock;
      
      function NewOr (i1, i2: PBool; q1 : PBool) return PBlock;
      
      function NewXor (i1, i2: PBool; q1 : PBool) return PBlock;
      
      function NewJump (i1 : PBool; q1 : PInt; offs : Integer) return PBlock;
      
   end Constructors;
   
   -----------------
   
   procedure DoTest;   
   
end bnew;
