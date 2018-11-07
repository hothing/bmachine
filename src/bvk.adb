with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvk is

   procedure AddInt(c : in out Context) is
      a : Integer := W32ToInt(c.pd1.all);
      b : Integer := W32ToInt(c.pd2.all);
   begin
      c.pd3.all := IntToW32(a + b);
   end AddInt;

   ------------
   -- DoTest --
   ------------

   procedure DoTest0 is
      c   : Context;
      tb, te : Ada.Real_Time.Time;
   begin

      -- Simulate a correct context
      c.pd1 := new Word32'(1);
      c.pd2 := new Word32'(5);
      c.pd3 := new Word32'(255);

      -- Cyclic test
      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         AddInt(c);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
   end DoTest0;

   procedure DoTest1 is
      c    : Context;
      pcv  : PtrConvert;
      pld  : PtrLocalData;
      pcp  : PtrMemSegment;
      pm   : PtrModule;
      p    : Process(100);
      tb, te : Ada.Real_Time.Time;
      a1, a2, a3 : Address;

      function GetCode return Byte is
         s    : constant Positive := Byte'Size / Byte'Size;
         npc  : Positive;
         code : Byte;
      begin
         if p.PC <= p.G.code'Last then
            code := p.G.code(p.PC);
         else
            code := 0;
         end if;
         npc := p.PC + s;
         if npc < p.G.code'Last then p.PC := npc; end if;
         return code;
      end GetCode;

      function GetAddr return Address is
         s : constant Positive := Word16'Size / Byte'Size;
         npc, adr : Address;
         pcv  : PtrConvert;
      begin
         npc := p.PC + s;
         if npc <= p.G.code'Last then
            pcv.pb := p.G.code(p.PC)'Access;
            adr := W32ToInt(pcv.pd.all);
         else
            adr := Address'First;
         end if;
         if npc < p.G.code'Last then p.PC := npc; end if;
         return adr;
      end getaddr;

   begin

      pcp := new MemorySegment(0 .. 1023);

      pm  := new Module(0, 1, 1023, 1023);
      pld := new LocalData(128, 128);

      p.PC := Address'First;
      p.G := pm;
      -- Link to a global data
      --pld.gData := pm.data'Access; -- Ooops! This compiler does not accept...

      -- Prepare code
      for i in pm.code'Range loop
         pm.code(i) := 0;
      end loop;

      pm.code(0) := 1;

      -- Cyclic test
      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         case GetCode is
            when 1 =>
               -- simulate an arguments read
               a1 := GetAddr;
               a2 := GetAddr;
               a3 := GetAddr;
               -- simulate a context preparation
               pcv.pb := pld.lData(4)'Access;
               c.pd1 := pcv.pd;
               pcv.pb := pld.lData(8)'Access;
               c.pd2 := pcv.pd;
               pcv.pb := pld.lData(12)'Access;
               c.pd3 := pcv.pd;
               AddInt(c);
            when others => null;
         end case;
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
   end DoTest1;

   procedure DoTest is
   begin
      DoTest0;
      DoTest1;
   end DoTest;

end bvk;
