with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvm1 is

   ------------
   -- DoTest --
   ------------

   procedure DoTest is
      ds : MemorySegment(1 .. 128);
      c16 : W16View;
      c32 : W32View;
      j : Integer;
      tb, te : Ada.Real_Time.Time;
      dt1 : Ada.Real_Time.Time_Span;
   begin

      c16.w := 16#1234#;

      ds(1..2) := c16.bytes(1 .. 2);
      ds(3..4) := MemorySegment'(c16.bytes);

      c32.bytes := ds(1 .. 4);

      Put_Line(c32.w'Image);

      tb := Ada.Real_Time.Clock;

      j := ds'First;
      for i in 1 .. 100_000_000 loop
         j := i mod (ds'Last - 3) + 1;
         --c32.bytes := ds(j .. j + 3);
         c32 := W32View'(t => True, bytes => ds(j .. j + 3));

         c32.w := c32.w - 1;
         j := j + 1; if j > ds'Last or j < ds'First then j := ds'First; end if;
      end loop;

      te := Ada.Real_Time.Clock;
      dt1 := te - tb;

      Put_Line(Duration'Image(To_Duration(dt1)));
   end DoTest;

   procedure DoTest2 is
      cs : PtrMemorySegment := new MemorySegment(1 .. 128);
      ds : PtrMemorySegment := new MemorySegment(1 .. 128);
      pc : Positive;
      cw : PtrWView;
      tb, te : Ada.Real_Time.Time;
      dt1 : Ada.Real_Time.Time_Span;

      procedure ExecInstr is
         code : Byte;
         adr1 : Positive := ds'First;
         adr2 : Positive := ds'First;
         adr3 : Positive := ds'First;

         pa1 : PtrWView := (t => B8, pb => ds(adr1)'Access);
         pa2 : PtrWView := (t => B8, pb => ds(adr2)'Access);
         pa3 : PtrWView := (t => B8, pb => ds(adr3)'Access);

         procedure AddInt is
         begin
            pa3.pw.all := pa1.pw.all + pa2.pw.all;
         end AddInt;

         procedure SubInt is
         begin
            pa3.pw.all := pa1.pw.all - pa2.pw.all;
         end SubInt;

         procedure MulInt is
         begin
            pa3.pw.all := pa1.pw.all * pa2.pw.all;
         end MulInt;

         procedure DivInt is
         begin
            pa3.pw.all := pa1.pw.all / pa2.pw.all;
         end DivInt;

         function GetAdr return Positive is
            s : constant Positive := Word16'Size / Byte'Size;
            npc, adr : Positive;
         begin
            npc := pc + s;
            if npc <= cs'Last and (pc <= cs'Last) then
               cw.pb := cs(pc)'Access;
               if (Integer(cw.pw.all) <= ds'Last)
                 and (Integer(cw.pw.all) >= ds'First)
               then
                  adr   := Positive(cw.pw.all);
               else
                  -- NB: exceptional case!
                  --     In real CPU the interrupt will activated
                  adr := ds'First;
               end if;
            else
               adr := ds'First;
            end if;
            if npc < cs'Last then pc := npc; end if;
            return adr;
         end GetAdr;

         procedure LoadArgs3 is
         begin
            pa1.pb := ds(GetAdr)'Access;
            pa2.pb := ds(GetAdr)'Access;
            pa3.pb := ds(GetAdr)'Access;
         end LoadArgs3;


         function GetCode return Byte is
            s : constant Positive := Byte'Size / Byte'Size;
            npc : Positive;
         begin
            npc := pc + s;
            if npc <= cs'Last then
               code := cs(pc);
            else
               code := cs(cs'Last);
            end if;
            if npc < cs'Last then pc := npc; end if;
            return code;
         end GetCode;

      begin
         case GetCode is
            when 1 => LoadArgs3; AddInt;
            when 2 => LoadArgs3; SubInt;
            when 3 => LoadArgs3; MulInt;
            when 4 => LoadArgs3; DivInt;
            when others => null;
         end case;
      end ExecInstr;

      s_b8  : constant Positive := Byte'Size / Byte'Size;
      s_w16 : constant Positive := Word16'Size / Byte'Size;
      s_w32 : constant Positive := Word32'Size / Byte'Size;
   begin

      declare
         i : Positive;
      begin

         i := cs'First;
         while i < (cs'Last - s_w32) loop
            cs(i) := Byte(i mod 5);
            i := i + s_b8;
            cw.pb := cs(i)'Access;
            cw.pw.all := Word16(ds'First + 1);
            i := i + s_w16;
            cw.pb := cs(i)'Access;
            cw.pw.all := Word16(ds'First + 2);
            i := i + s_w16;
            cw.pb := cs(i)'Access;
            cw.pw.all := Word16(ds'First + 3);
            i := i + s_w16;
         end loop;
      end;

      tb := Ada.Real_Time.Clock;

      pc := cs'First;
      for i in 1 .. 100_000_000 loop
         ExecInstr;
      end loop;

      te := Ada.Real_Time.Clock;
      dt1 := te - tb;

      Put_Line(Duration'Image(To_Duration(dt1)));
   end DoTest2;
   -- Result = (mode : default; time : 2500 ms / 1e8 instr)
   -- Result = (mode : optimize; time : 350 ms / 1e8 instr)
end bvm1;
