with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package bvmkp is

   type DataFormat is (B8, W8, W16, W32, W64, W80);
   
   subtype Byte is Unsigned_8;
   subtype Word16 is Unsigned_16;
   subtype Word32 is Unsigned_32;
   subtype Word64 is Unsigned_64;
   
   subtype Address is Word32 range 0 .. Word32'Last / (2 * Word32'Size / Byte'Size);
   
   type Bool is new Boolean;
   for Bool'Size use 8;
   
   type Bit is new Boolean;
   for Bit'Size use 1;
   
   subtype BitAddress is Byte range 0 .. 31;
   
   type BitField is array(BitAddress) of Bit;
   for BitField'Size use Word32'Size;
   pragma Pack (BitField);
   
   type W32Value(t : Boolean := False) is record
      case t is
         when False => w : Word32;
         when True => x : BitField;
      end case;
   end record;
   pragma Unchecked_Union(W32Value);
   pragma Pack(W32Value);
   
   type MemoryBlock is array (Address range <>) of aliased W32Value;
   
   type PtrBool is access all Bool;
   type PtrWord32 is access all Word32;
   type PtrW32Value is access all W32Value;
   
   type Module;
   type PtrModule is access Module;
   type RefModule is access PtrModule;
   
   type LocalData;
   type PtrLocalData is access all LocalData;  
   
   type LocalData(ds : Address; N : Address; gData   : Boolean) is record
      sData   : MemoryBlock(Address'First .. ds); -- static data
                                                   -- NB: first N words are reserved
                                                   -- they are used as instruction registers
                                                  -- and for parameters
      case gData is
         when false => upLink  : PtrLocalData;  -- link to a parent function variables
         when true => null;
      end case;      
   end record;
   
   type PhiFunction;
   type PtrPhiFunction is access PhiFunction;
   
   subtype FuncArrIndex is Address range 0 .. 64;
   type FuncArray is array (FuncArrIndex) of PtrPhiFunction;   
   type FuncMap is array(FuncArrIndex range <>) of FuncArrIndex;
   
   type PhiCodeFormat is (PCF_REG3, PCF_STACK, PCF_BITS);
   for PhiCodeFormat'Size use 4;
   
   type PhiCode(b : PhiCodeFormat := PCF_REG3) is record
      code : Byte;
      case b is
         when PCF_REG3 =>
            reg1 : Byte;
            reg2 : Byte;
            reg3 : Byte;
         when PCF_STACK =>
            --sel  : Byte;
            sel  : Word16;
         when PCF_BITS =>
            regb : Byte;
            --bita : BitAddress;
      end case;
   end record;
   pragma Pack(PhiCode);
   pragma Unchecked_Union(PhiCode);
   for PhiCode'Size use Word32'Size;
   
   type PhiResult is (FcExec, FcExit, FcFailure);
   type PhiCodeLine is array(Address range <>) of PhiCode;
   
   type PhiFunction(cs : Address) is record
      frame  : PtrLocalData; -- reference to the parameters and static data
                             -- I use the reference because of hot-programm change
                             -- Of course, it's not solid
      code   : PhiCodeLine(Address'First .. cs); -- mu-code
      PC     : Address; -- program counter / instruction pointer
      res    : PhiResult;
      
      subfc  : FuncArray;
   end record;     
   

   subtype ModArrIndex is Address range 0 .. 64;   
   type ModuleRefArray is array (ModArrIndex) of RefModule;
   type PtrModRefArray is access ModuleRefArray;
   
   
   
   type AddrArray is array (Address range <>) of Address;
   
   type VariableAddress is record
      modId  : ModArrIndex;
      varId  : Address range 0 .. 65535;
   end record;
   for VariableAddress'Size use 32;
   pragma Pack(VariableAddress);
   
   type Module(ds : Address; ev, ef : Address) is record
      expv   : AddrArray(Address'First .. ev); -- exported variables table (id -> offset)
      expf   : FuncMap(FuncArrIndex'First .. ef); -- exported functions (id -> id)
      
      -- PRIVATE PART -- 
      data   : aliased LocalData(ds, ds, True);
      mLink  : ModuleRefArray;
      func   : FuncArray;
   end record;
      
   procedure call (func : PtrPhiFunction; fumod : PtrModule);
   
   function IntToW32 is new
     Ada.Unchecked_Conversion(Integer, Word32);

   function W32ToInt is new
     Ada.Unchecked_Conversion(Word32, Integer);

   function W32ToFloat is new
     Ada.Unchecked_Conversion(Word32, Float);

   function FloatToW32 is new
     Ada.Unchecked_Conversion(Float, Word32);
   
   function VarAddrToW32 is new
     Ada.Unchecked_Conversion(VariableAddress, Word32);
   
   function W32ToVarAddr is new
     Ada.Unchecked_Conversion(Word32, VariableAddress);
      
   procedure DoTest;
 

end bvmkp;
