with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvk is

   procedure IncR(c : in out Context) is
   begin
      if c.ri < RStackIndex'Last then
         c.ri := c.ri + 1;
      end if;
   end IncR;
   --pragma Inline_Always(IncR);

   procedure DecR(c : in out Context) is
   begin
      if c.ri > RStackIndex'First then
         c.ri := c.ri - 1;
      end if;
   end DecR;
   --pragma Inline_Always(DecR);

   function GetCode(c : in out Context) return Byte is
      s    : constant Positive := Byte'Size / Byte'Size;
      npc  : Positive;
      code : Byte;
   begin
      if c.PC <= c.G.code'Last then
         code := c.G.code(c.PC);
      else
         code := 0;
      end if;
      npc := c.PC + s;
      if npc < c.G.code'Last then c.PC := npc; end if;
      return code;
   end GetCode;

   procedure LLW(c : in out Context) is
      p2v  : PtrConvert;
      adr  : Address;
   begin
      p2v.pb := c.G.code(c.PC)'Access;
      adr  := W32ToInt(p2v.pd.all);
      if adr >= c.L.lData'First and adr <= (c.L.lData'Last - 4) then
         p2v.pb := c.L.lData(adr)'access;
         c.ry(c.ri) := p2v.pd.all;
      end if;
   end LLW;

   procedure SLW(c : in out Context) is
      p2v  : PtrConvert;
      adr  : Address;
   begin
      p2v.pb := c.G.code(c.PC)'Access;
      adr  := W32ToInt(p2v.pd.all);
      if adr >= c.L.lData'First and adr <= (c.L.lData'Last - 4) then
         p2v.pb := c.L.lData(adr)'access;
         p2v.pd.all := c.ry(c.ri);
      end if;
   end SLW;

   procedure ADDI(c : in out Context) is
      p2v  : PtrConvert;
      a,b  : Integer;
   begin
      a := W32ToInt(c.ry(c.ri)); DecR(c);
      b := W32ToInt(c.ry(c.ri));
      c.ry(c.ri) := IntToW32(a + b);
   end ADDI;

   ------------
   -- DoTest --
   ------------



   procedure DoTest1 is
      c    : Context;
      pcv  : PtrConvert;
      pld  : PtrLocalData;
      pcp  : PtrMemSegment;
      pm   : PtrModule;
      tb, te : Ada.Real_Time.Time;

   begin

      pcp := new MemorySegment(0 .. 1023);

      pm  := new Module(1023, 1023);
      pld := new LocalData(128, 128);

      c.PC := Address'First;
      c.G := pm;
      c.L := pld;
      c.ri := RStackIndex'First;

      -- Link to a global data
      --pld.gData := pm.data'Access; -- Ooops! This compiler does not accept...

      -- Prepare code
      for i in pm.code'Range loop
         pm.code(i) := Byte(i mod 4);
      end loop;
      pm.code(0) := 1;

      -- prepare local data
      for i in pld.lData'Range loop
         pld.lData(i) := 0;
      end loop;

      -- Cyclic test
      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         case GetCode(c) is
            when 1 => LLW(c);
            when 2 => SLW(c);
            when 3 => ADDI(c);
            when others => null;
         end case;
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
   end DoTest1;

   procedure DoTest2 is
      a, b, c : Bit;
   begin
      a := true;
      b := false;
      c := a and b;
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
