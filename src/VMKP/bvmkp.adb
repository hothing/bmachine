with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body bvmkp is

   ----------
   -- call --
   ----------

   procedure call (self : in out PhiFunction) is
      c : PhiCode;
   begin
      while self.PC in self.code'Range loop
         c := self.code(self.PC);
         case c.code is
         when 16#00# => null;
         when 16#01# =>
            self.frame.sData(Word32(c.reg3)).w :=
              self.frame.sData(Word32(c.reg1)).w;
         when 16#02# =>
            self.frame.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(self.frame.sData(Word32(c.reg1)).w)
                       +
                         W32ToInt(self.frame.sData(Word32(c.reg2)).w));
         when 16#03# =>
            self.frame.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(self.frame.sData(Word32(c.reg1)).w)
                       -
                         W32ToInt(self.frame.sData(Word32(c.reg2)).w));
         when 16#04# =>
            self.frame.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(self.frame.sData(Word32(c.reg1)).w)
                       *
                         W32ToInt(self.frame.sData(Word32(c.reg2)).w));
         when 16#05# =>
            self.frame.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(self.frame.sData(Word32(c.reg1)).w)
                       /
                         W32ToInt(self.frame.sData(Word32(c.reg2)).w));
            when 16#07# =>
               if self.atop >= 1 then
                  self.accu(self.atop - 1).w :=
                    IntToW32(
                             W32ToInt(self.accu(self.atop - 1).w)
                             +
                               W32ToInt(self.accu(self.atop).w)
                            );
                  self.atop := self.atop - 1;
               else
                  -- TODO: reaction
                  null;
               end if;
            when 16#08# =>
               if self.atop < self.accu'Last then
                  self.atop := self.atop + 1;
                  self.accu(self.atop - 1).w := 1;
               end if;

            when 16#10# =>
               declare
                  old_pc : Address;
               begin
                  old_pc := self.PC;
                  self.PC := self.PC + (self.frame.sData(Word32(c.reg1)).w +
                                          self.frame.sData(Word32(c.reg2)).w * 255);
                  if not (self.PC in self.code'Range) then
                     self.PC := old_pc;
                     exit;
                  end if;
               end;
            when others => null;
         end case;

         self.PC := self.PC + 1;
      end loop;
   end call;

   ------------
   -- DoTest --
   ------------

   testInstructions : constant Integer := 100_000_000;

   procedure DoTest is
      tb, te : Ada.Real_Time.Time;
      m : Integer;
      pf : PtrPhiFunction;
   begin
      pf := new PhiFunction(8);
      pf.frame := new LocalData(128, 16, false);
      pf.frame.sData(0).w := 0;
      pf.frame.sData(1).w := 0;

      for i in pf.code'Range loop
         case i mod 3 is
            when 0 =>
               pf.code(i).code := 16#02#;
               pf.code(i).reg1 := 1;
               pf.code(i).reg2 := 0;
               pf.code(i).reg3 := 0;
            when 1 =>
               pf.code(i).code := 16#08#; -- PUSH 1
            when 2 =>
               pf.code(i).code := 16#07#; -- ADDI
            when others =>
               null;
         end case;
      end loop;
      pf.PC := pf.code'First;

      M := testInstructions / Integer(pf.cs);
      tb := Ada.Real_Time.Clock;
      for i in 1 .. M loop
         call(pf.all);
      end loop;
      te := Ada.Real_Time.Clock;

      Put("Duration: ");
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put("Result: ");
      Put_Line(Word32'Image(pf.accu(0).w));

   end DoTest;

end bvmkp;
