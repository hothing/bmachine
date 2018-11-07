with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package bvk is

   type DataFormat is (W8, W16, W32, W64, W80);

   subtype Byte is Unsigned_8;
   subtype Word16 is Unsigned_16;
   subtype Word32 is Unsigned_32;
   subtype Word64 is Unsigned_64;
   
   subtype Address is Integer;
   
   type PtrByte is access all Byte;
   type PtrWord16 is access all Word16;
   type PtrWord32 is access all Word32;
   type PtrWord64 is access all Word64;
   
   type PtrConvert (t : DataFormat := W8) is record
      case t is
         when W8 => pb : PtrByte;
         when W16 => pw: PtrWord16;
         when W32 => pd : PtrWord32;
         when W64 => pq : PtrWord64;
         when W80 => pl : PtrWord64;  -- FIXME later!
      end case;
   end record;
   pragma Unchecked_Union(PtrConvert);
   
   type MemorySegment is array (Address range <>) of aliased Byte;
   type PtrMemSegment is access all MemorySegment;
   
   type Module;
   type PtrModule is access Module;
   type RefModule is access PtrModule;
   
   
   
   subtype ModArrIndex is Integer range 0 .. 64;   
   type ModuleRefArray is array (ModArrIndex range <>) of RefModule;
   type PtrModRefArray is access ModuleRefArray;
   
   subtype EntArrIndex is Integer range 0 .. 64;
   type EntriesArray is array (EntArrIndex range <>) of Address;   
   type PtrEntriesArray is access EntriesArray;
   
   type Module(mi : ModArrIndex; ei : EntArrIndex; ds : Address; cs : Address) is record
      data   : aliased MemorySegment(Address'First .. ds);
      mLink  : ModuleRefArray(ModArrIndex'First .. mi);
      pt     : EntriesArray(EntArrIndex'First .. ei);          
      code   : MemorySegment(Address'First .. cs);
      cp     : PtrMemSegment; -- pointer to a global constants segment  
   end record;
   
   type LocalData;
   type PtrLocalData is access LocalData;
   
   type LocalData(ds : Address; ps : Address) is record
      lData       : aliased MemorySegment(Address'First .. ds); -- local data
      cParam      : MemorySegment(Address'First .. ps); -- parameters for a function call
      gData       : PtrMemSegment; -- reference to a module data
      upLink      : PtrLocalData;  -- link to a parent function P-stack 
      dynLink     : PtrLocalData; -- link to a local data of a callee
      --retPC       : Address;  -- a code pointer for return (should be removed???)
   end record;
   
   type Context is record
      data  : PtrLocalData;
      pb1   : PtrByte;
      pb2   : PtrByte;
      pb3   : PtrByte;
      pw1   : PtrWord16;
      pw2   : PtrWord16;
      pw3   : PtrWord16;
      pd1   : PtrWord32;
      pd2   : PtrWord32;
      pd3   : PtrWord32;
      pq1   : PtrWord64;
      pq2   : PtrWord64;
      pq3   : PtrWord64;
      cb    : Byte;
      ci    : Integer;
      cf    : Float;
   end record;   
   type PtrContext is access Context;   
   
   type AddrArray is array (Address range <>) of Address;
      
   type Process(ss : Address) is record
      G   : PtrModule; -- pointer to a Module 
      L   : PtrLocalData; -- pointer to a P-stack
      PC  : Address; -- the actual code address in the Module.code 
      rs  : AddrArray(Address'First .. ss); -- a return call stack
      rp  : Address; -- a top of RS
   end record;
   
   function IntToW32 is new
     Ada.Unchecked_Conversion(Integer, Word32);

   function W32ToInt is new
     Ada.Unchecked_Conversion(Word32, Integer);

   function W32ToFloat is new
     Ada.Unchecked_Conversion(Word32, Float);

   function FloatToW32 is new
     Ada.Unchecked_Conversion(Float, Word32);
   
   procedure DoTest;
   
end bvk;
