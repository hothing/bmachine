package bvm is

   type DataType is (TBool, TInt, TFloat);
   
   type DatFormat is (W16, W32, W64, F32, F64, F80);
   
   type BoolArray is array (Positive range <>) of Boolean;
   
   type IntArray is array (Positive range <>) of Integer;
   
   type FloatArray is array(Positive range <>) of Float; 
   
   type BCode is (
                  Nop,
                  CopyI,
                  AddI,
                  SubI,
                  MulI,
                  DivI,
                  ModI,
                  BNot,
                  BAsn,
                  BAnd,
                  BOr,
                  BXor,
                  BSet,
                  BRes,
                  CmpEQI,
                  CmpNEI,
                  CmpGTI,
                  CmpGEI,
                  CmpLTI,
                  cmpLEI,
                  ParamB,
                  ParamI,
                  ParamF,
                  Call,
                  Ret,
                  Jmp
                 );
   
   type Instruction is record
      code  :  BCode;
      arg1  :  Positive;
      arg2  :  Positive;
      rest  :  Positive;
   end record;
   
   type Code is array(Positive range <>) of Instruction;
   
   type Frame (sb, si, sf: Positive)  is record
      mb : BoolArray(Positive'First .. sb);
      mi : IntArray(Positive'First .. si);
      mf : FloatArray(Positive'First .. sf);
   end record;
   
   type PFrame is access Frame;
   
   type Routine;
   type PRoutine is access Routine;
   
   type Routines is array(Positive range Positive'First .. 64) of PRoutine;
   
   type Routine (sc : Positive) is tagged record
      clink  : Routines;  
      calee  : PRoutine;
      frm    : PFrame;      
      cx     : Code(Positive'First .. sc);
      
      r1, r2, r3, rx : Positive; --  address registers
                                 --  TODO: hide
                                 
   end record;
   
   procedure Run(e : Routine'Class);
   function Check(e : Routine'Class) return Boolean;
   procedure DoCycle(e : Routine);
   function DoCheck(e : Routine) return Boolean;
   
   
   procedure DoTest;

end bvm;
