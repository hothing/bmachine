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
      pc : PtrWView;
      j : Integer;
      tb, te : Ada.Real_Time.Time;
      dt1 : Ada.Real_Time.Time_Span;

      procedure AddInt(a1, a2, ar : Positive) is
         pa1 : PtrWView := (t => B8, pb => ds(a1)'Access);
         pa2 : PtrWView := (t => B8, pb => ds(a2)'Access);
         pr  : PtrWView := (t => B8, pb => ds(ar)'Access);
      begin
         pr.pw.all := pa1.pw.all + pa2.pw.all;
      end AddInt;

   begin

      ds(1) := 0;
      ds(2) := 1;
      ds(3) := 0;
      ds(4) := 1;

      pc.pb := ds(1)'Access;

      Put_Line(pc.pw.all'Image);

      for i in cs'Range loop
         cs(i) := 1; -- add int instruction
      end loop;


      tb := Ada.Real_Time.Clock;

      j := ds'First;
      for i in 1 .. 100_000_000 loop
         case cs(cs'First + (i mod cs'Last)) is
            when 1 => AddInt(j + 5, j + 3, j);
            when others => null;
         end case;
         j := j + 2;
         if j > (ds'Last - 8) or j < ds'First then j := ds'First; end if;
      end loop;

      te := Ada.Real_Time.Clock;
      dt1 := te - tb;

      Put_Line(Duration'Image(To_Duration(dt1)));
   end DoTest2;
   -- Result = (mode : default; time : 2500 ms / 1e8 instr)
   -- Result = (mode : optimize; time : 350 ms / 1e8 instr)
end bvm1;
