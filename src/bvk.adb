with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvk is

   function call (self : in out MuFunction) return FunctionResult is
   begin
      return Failure;
   end call;

   function execCode (self :  in out MuFunction) return FunctionResult is
   begin
      return Failure;
   end execCode;


   ------------
   -- DoTest --
   ------------

   procedure DoTest1 is
      pld  : PtrLocalData;
      pcp  : PtrMemSegment;
      pm   : PtrModule;
   begin

      pcp := new MemorySegment(0 .. 1023);

      pm  := new Module(1023, 16, 256);
      pm.cp := pcp;

      pld := new LocalData(128, 128);

   end DoTest1;

   procedure DoTest2 is
      a : MemorySegment(0 .. 256);

      type W16Bytes is record
         h : Byte;
         l : Byte;
      end record;
      for W16Bytes'Size use 16;
      pragma Pack(W16Bytes);

      type W32Bytes is new MemorySegment(0..3);
      for W32Bytes'Size use 32;
      pragma Pack(W32Bytes);

      function W16ToBytes is new
           Ada.Unchecked_Conversion(Word16, W16Bytes);

      function BytesToW16 is new
           Ada.Unchecked_Conversion(W16Bytes, Word16);

      function W32ToBytes is new
           Ada.Unchecked_Conversion(Word32, W32Bytes);

      function BytesToW32 is new
           Ada.Unchecked_Conversion(W32Bytes, Word32);

      function ReadW16(m : in out MemorySegment;
                       offset : Address)
                       return Word16
      is
         v : W16Bytes;
      begin
         v.h := m(offset);
         v.l := m(offset + 1);
         return BytesToW16(v);
      end ReadW16;

      function ReadW32(m : in out MemorySegment;
                       offset : Address)
                       return Word32
      is
      begin
         return BytesToW32(W32Bytes(m(offset .. offset + 3)));
      end ReadW32;
      pragma Inline(ReadW32);

      procedure WriteW32(m : in out MemorySegment;
                         offset : Address;
                         val : Word32)
      is
      begin
         m(offset .. offset + 3) := MemorySegment(W32ToBytes(val));
      end WriteW32;
      pragma Inline(WriteW32);

      procedure OpArgs(arg : in out MemorySegment) is
         p2v : PtrConvert(B8);
      begin
         arg(arg'First + 0) := 1;
         --p2v.pb := arg(0)'Access;
      end OpArgs;

      type MuFunc(lds   : Integer) is record
         codes : MemorySegment(0..255);
         pc    : Integer;
      end record;

      procedure SimLocalData(n : in Integer) is
         ds : MemorySegment(0 .. n);
      begin
         ds(0) := 129;
      end SimLocalData;

      procedure exec(mf : in out MuFunc) is
         ds : MemorySegment(0 .. mf.lds);
         p2v : PtrConvert;
      begin
         ds(0) := 129;
      end exec;

      function AddW32(v1, v2: Word32) return Word32 is
      begin
         return v1 + v2;
      end AddW32;

      func1 : MuFunc(512);
      func2 : MuFunc(16);
      vw    : Word16 := 123;
      vd1, vd2    : Word32 := 415;
      tb, te : Ada.Real_Time.Time;
   begin
      OpArgs( a(2..3) );
      exec(func1);
      exec(func2);

      tb := Ada.Real_Time.Clock;

      WriteW32(a, a'First, 0);
      WriteW32(a, a'First + Word32'Size / Byte'Size, 1);
      for i in 1 .. 100_000_000 loop
         vd1 := ReadW32(a, a'First);
         vd2 := ReadW32(a, a'First + Word32'Size / Byte'Size);
         WriteW32(a, a'First, vd1 + vd2);
      end loop;
      te := Ada.Real_Time.Clock;

      vd1 := ReadW32(a, a'First);
      Put_Line(Word32'Image(vd1));
      Put_Line(Duration'Image(To_Duration(te - tb)));

      tb := Ada.Real_Time.Clock;

      vd1 := 0;
      vd2 := 1;
      for i in 1 .. 100_000_000 loop
         --vd1 := vd1 + vd2;
         vd2 := Word32(i mod 2);
         vd1 := AddW32(vd1, vd2);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(vd1));
   end DoTest2;

   procedure DoTest is
   begin
      DoTest1;
      DoTest2;
   end DoTest;

end bvk;

-- [DoTest0] elapsed time: 00.45s (mode:default; 1e8 instr)
-- [DoTest0] elapsed time: 00.21s (mode:optimize; 1e8 instr)
-- [DoTest1] elapsed time: 00.67s (mode:default; 1e8 instr)
-- [DoTest1] elapsed time: 00.16s (mode:optimize; 1e8 instr)

-- [DoTest0] elapsed time: 00.35s (mode:default, w/o checks; 1e8 instr)
-- [DoTest0] elapsed time: 00.03s (mode:optimize, w/o checks; 1e8 instr)
-- [DoTest1] elapsed time: 00.41s (mode:default, w/o checks; 1e8 instr)
-- [DoTest1] elapsed time: 00.15s (mode:optimize, w/o checks; 1e8 instr)
