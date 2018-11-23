with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body bvmkp is

   ----------
   -- call --
   ----------

   procedure call (self : in out PhiFunction) is
      c : PhiCode;
      s : PtrLocalData;
      rca, rcb : Boolean;
      -- Ariphmetic stack
      accu   : MemoryBlock(1 .. 7);
      atop   : Address := 0;

      -- Instructions

      procedure Dup is
      begin
         if atop < accu'Last and atop >= accu'First then
            atop := atop + 1;
            accu(atop).w := accu(atop - 1).w;
         else
            rcb := false;
         end if;
      end Dup;

      procedure Drop is
      begin
         if atop >= accu'First then
            accu(atop).w := 0;
            atop := atop - 1;
         end if;
      end Drop;

      procedure PushZero is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := 0;
         else
            rcb := false;
         end if;
      end PushZero;

      procedure PushOne is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := 1;
         else
            rcb := false;
         end if;
      end PushOne;

      procedure PushShortInt is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := Word32(c.sel);
         else
            rcb := false;
         end if;
      end PushShortInt;

      procedure IncInt is
      begin
         accu(atop).w :=
           IntToW32(W32ToInt(accu(atop).w) + 1);
      end IncInt;

      procedure DecInt is
      begin
         accu(atop).w :=
           IntToW32(W32ToInt(accu(atop).w) - 1);
      end DecInt;

      procedure AddInt is
      begin
         if atop > accu'First then
            accu(atop - 1).w :=
              IntToW32(
                       W32ToInt(accu(atop).w)
                       +
                         W32ToInt(accu(atop - 1).w)
                      );
            atop := atop - 1;
         else
            rcb := false;
         end if;
      end AddInt;

      procedure SubInt is
      begin
         if atop > accu'First then
            accu(atop - 1).w :=
              IntToW32(
                       W32ToInt(accu(atop).w)
                       -
                         W32ToInt(accu(atop - 1).w)
                      );
            atop := atop - 1;
         else
            rcb := false;
         end if;
      end SubInt;

      procedure MulInt is
      begin
         if atop > accu'First then
            accu(atop - 1).w :=
              IntToW32(
                       W32ToInt(accu(atop).w)
                       *
                         W32ToInt(accu(atop - 1).w)
                      );
            atop := atop - 1;
         else
            rcb := false;
         end if;
      end MulInt;

      procedure DivInt is
      begin
         if atop > accu'First then
            if W32ToInt(accu(atop).w) /= 0 then
               accu(atop - 1).w :=
                 IntToW32(
                          W32ToInt(accu(atop - 1).w)
                          /
                            W32ToInt(accu(atop).w)
                         );
               atop := atop - 1;
            else
               -- TODO : reaction
               rcb := false;
            end if;
         end if;
      end DivInt;

      procedure ModUInt is
      begin
         if atop > accu'First then
            if accu(atop).w /= 0 then
               accu(atop - 1).w :=
                 accu(atop - 1).w
                          mod accu(atop).w;
               atop := atop - 1;
            else
               -- TODO : reaction
               rcb := false;
            end if;
         end if;
      end ModUInt;

      procedure UseLocal is
      begin
         s := self.frame;
      end UseLocal;

      procedure UseUplevel( lev : Byte ) is
      begin
         s := self.frame;
         for i in lev .. 0 loop
            if s /= null then
               s := s.upLink;
            end if;
         end loop;
         if s = null then
            -- TODO : reaction
            rcb := false;
         end if;
      end UseUplevel;

      procedure Load is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := s.sData(Word32(c.sel)).w;
         end if;
      end Load;

      procedure Store is
      begin
         if atop in accu'Range then
            s.sData(Word32(c.sel)).w := accu(atop).w;
         end if;
      end Store;

      -- 3Reg integer operations
      procedure R3_AddInt is
      begin
         s.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(s.sData(Word32(c.reg1)).w)
                       +
                         W32ToInt(s.sData(Word32(c.reg2)).w));
      end R3_AddInt;

      procedure R3_SubInt is
      begin
         s.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(s.sData(Word32(c.reg1)).w)
                       -
                         W32ToInt(s.sData(Word32(c.reg2)).w));
      end R3_SubInt;

      procedure R3_MulInt is
      begin
         s.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(s.sData(Word32(c.reg1)).w)
                       *
                         W32ToInt(s.sData(Word32(c.reg2)).w));
      end R3_MulInt;

      procedure R3_DivInt is
      begin
         s.sData(Word32(c.reg3)).w :=
              IntToW32(W32ToInt(s.sData(Word32(c.reg1)).w)
                       /
                         W32ToInt(s.sData(Word32(c.reg2)).w));
      end R3_DivInt;

      procedure R3_Copy is
      begin
         s.sData(Word32(c.reg3)).w := s.sData(Word32(c.reg1)).w;
      end R3_Copy;

      -- Flow control

      procedure JumpUncond is
         old_pc : Address;
      begin
         old_pc := self.PC;
         self.PC := self.PC + Address(c.sel);
         if not (self.PC in self.code'Range) then
            self.PC := old_pc;
            rcb := false;
         end if;
      end JumpUncond;

   begin
      UseLocal;
      self.PC := self.code'First;
      rcb := true;
      rca := self.PC in self.code'Range;
      while rca and rcb loop
         c := self.code(self.PC);
         case c.code is
         when 16#00# => null;
         when 16#01# => R3_Copy;
         when 16#02# => R3_AddInt;
         when 16#03# => R3_SubInt;
         when 16#04# => R3_MulInt;
         when 16#05# => R3_DivInt;
         when 16#10# => UseLocal;
         when 16#11# => UseUplevel(c.reg1);
         when 16#12# => Load;
         when 16#13# => Store;
         when 16#14# => PushZero;
         when 16#15# => PushOne;
         when 16#16# => PushShortInt;
         when 16#17# => Dup;
         when 16#18# => Drop;
         when 16#22# => AddInt;
         when 16#23# => SubInt;
         when 16#24# => MulInt;
         when 16#25# => DivInt;
         when 16#26# => ModUInt;
         when 16#F0# => JumpUncond;
         when others => null;
         end case;
         self.PC := self.PC + 1;
         rca := self.PC in self.code'Range;
      end loop;
      if not rcb then
         self.res := FcFailure;
      end if;
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

      declare
         i : Address;
      begin
         i := pf.code'First;
         -- PUSH 0
         pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#14#, sel => 0);
         i := i + 1;
         while i < pf.code'Last - 2 loop
            -- PUSH 1
            pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#15#, sel => 0);
            i := i + 1;
            -- ADDI
            pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#22#, sel => 0);
            i := i + 1;
         end loop;
         -- UL
         pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#10#, sel => 0);
         i := i + 1;
         -- STORE
         pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#13#, sel => 0);
         i := i + 1;
      end;


      M := testInstructions / Integer(pf.cs);
      tb := Ada.Real_Time.Clock;
      for i in 1 .. M loop
         call(pf.all);
      end loop;
      te := Ada.Real_Time.Clock;

      Put("Duration: ");
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put("Result: ");
      Put_Line(Word32'Image(pf.frame.sData(0).w));
   end DoTest;

end bvmkp;
