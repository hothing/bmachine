with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package bvk is

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
   
   type W32Bits(t : Boolean := False) is record
      case t is
         when False => w : Word32;
         when True => x : BitField;
      end case;
   end record;
   pragma Unchecked_Union(W32Bits);
   pragma Pack(W32Bits);
      
   type MemorySegment is array (Address range <>) of aliased Word32;
   type PtrMemSegment is access all MemorySegment;
   
   type PtrBool is access all Bool;
   type PtrWord32 is access all Word32;
   
   type Module;
   type PtrModule is access Module;
   type RefModule is access PtrModule;
   
   type LocalData;
   type PtrLocalData is access LocalData;  
   
   type Instruction is tagged null record;
   type PtrInstruction is access Instruction'Class;
   
   procedure exec(ins : in out Instruction'Class);
   procedure impl_opcode(ins : in out Instruction);
   
   type FunctionResult is (Stop, Run, Finish, Failure);
   type MuCodeLine is array(Address range <>) of PtrInstruction;
   
   type MuFunction(cs : Address) is record
      frame  : PtrLocalData;                                                                                               
      code   : MuCodeLine(Address'First .. cs); -- mu-code
      PC     : Address; -- program counter / instruction pointer
   end record;
   type PtrMuFunction is access MuFunction;
   
   procedure call (self : in out MuFunction);
   function execCode (self :  in out MuFunction) return FunctionResult;
   
   subtype ModArrIndex is Address range 0 .. 64;   
   type ModuleRefArray is array (ModArrIndex) of RefModule;
   type PtrModRefArray is access ModuleRefArray;
   
   subtype FuncArrIndex is Address range 0 .. 64;
   type FuncArray is array (FuncArrIndex) of PtrMuFunction;   
   type FuncMap is array(FuncArrIndex range <>) of FuncArrIndex;
   
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
      data   : MemorySegment(Address'First .. ds);
      mLink  : ModuleRefArray;
      func   : FuncArray;
      cp     : PtrMemSegment; -- pointer to a global constants segment  
   end record;
   
   
   type LocalData(ds : Address; N : Address) is record
      sData   : MemorySegment(Address'First .. ds); -- static data
                                                   -- NB: first N words are reserved
                                                   -- they are used as instruction registers
                                                   -- and for parameters
      gData   : PtrModule; -- reference to a own module
       
      upLink  : PtrLocalData;  -- link to a parent function variables 
   end record;
   
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
   
end bvk;
