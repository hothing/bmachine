with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package bvm1 is

   subtype Byte is Unsigned_8;
   subtype Word16 is Unsigned_16;
   subtype Word32 is Unsigned_32;
   subtype Address is Word32;
   
   type MemorySegment is array (Positive range <>) of aliased Byte;
   
   subtype W16Bytes is MemorySegment (1..2);
   subtype W32Bytes is MemorySegment (1..4);
   subtype W64Bytes is MemorySegment (1..8);
   
   function ToWord is new
     Ada.Unchecked_Conversion(Integer, Word32);

   function ToInt is new
     Ada.Unchecked_Conversion(Word32, Integer);

   function ToFloat is new
     Ada.Unchecked_Conversion(Word32, Float);

   function ToWord is new
     Ada.Unchecked_Conversion(Float, Word32);
   
   type W16View (t : Boolean := False) is record
      case t is
         when False => w : Word16;
         when True => bytes: W16Bytes;
      end case;
   end record;
   pragma Unchecked_Union(W16View);
   
   type W32View (t : Boolean := False) is record
      case t is
         when False => w : Word32;
         when True => bytes: W32Bytes;
      end case;
   end record;
   pragma Unchecked_Union(W32View);
   
   type PtrByte is access all Byte;
   type PtrWord16 is access all Word16;
   type PtrWord32 is access all Word32;
   
   type PtrMemorySegment is access MemorySegment;
   
   type DataFormat is (B8, W16, W32);
   
   type PtrWView (t : DataFormat := B8) is record
      case t is
         when B8 => pb : PtrByte;
         when W16 => pw: PtrWord16;
         when W32 => pd : PtrWord32;            
      end case;
   end record;
   pragma Unchecked_Union(PtrWView);
   
   procedure DoTest;
   procedure DoTest2;
   
end bvm1;
