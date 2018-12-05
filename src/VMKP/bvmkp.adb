with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body bvmkp is

   ----------
   -- call --
   ----------

   procedure call (func : PtrPhiFunction; fumod : PtrModule) is
      f : PtrPhiFunction;
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

      procedure CopyW is
      begin
         s.sData(Word32(c.reg3)).w := s.sData(Word32(c.reg1)).w;
      end CopyW;

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
         s := f.frame;
      end UseLocal;

      procedure UseUplevel( lev : Byte ) is
      begin
         s := f.frame;
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

      -- Flow control

      procedure JumpUncond is
         old_pc : Address;
      begin
         old_pc := f.PC;
         f.PC := f.PC + Address(c.sel);
         if not (f.PC in f.code'Range) then
            f.PC := old_pc;
            rcb := false;
         end if;
      end JumpUncond;

      procedure CallModuleFc is
         fn : PtrPhiFunction;
      begin
         rcb := false;
         if Address(c.reg1) in f.subfc'Range then
            fn := fumod.func(Address(c.reg1));
            if fn /= null then
               call(fn, fumod);
               rcb := true;
            end if;
         end if;
      end CallModuleFc;

      procedure CallLocalFc is
         fn : PtrPhiFunction;
      begin
         rcb := false;
         if Address(c.reg1) in f.subfc'Range then
            fn := f.subfc(Address(c.reg1));
            fn.frame.upLink := func.frame;
            if fn /= null then
               call(fn, fumod);
               rcb := true;
            end if;
         end if;
      end CallLocalFc;

      pragma Inline(Dup, Drop, PushOne, PushZero, PushShortInt);
      pragma Inline(AddInt,SubInt, MulInt,DivInt,ModUInt);
   begin
      f := func;
      if f /= null then
         UseLocal;
         f.PC := f.code'First;
         rcb := true;
         rca := f.PC in f.code'Range;
         while rca and rcb loop
            c := f.code(f.PC);
            case c.code is
               when 16#00# => null;
               when 16#01# => CopyW;
               when 16#02# => PushZero;
               when 16#03# => PushOne;
               when 16#04# => PushShortInt;
               when 16#05# => Dup;
               when 16#06# => Drop;
               when 16#07# => IncInt;
               when 16#08# => DecInt;
               when 16#10# => Load;
               when 16#12# => Store;
               when 16#14# => UseLocal;
               when 16#15# => UseUplevel(c.reg1);
               when 16#20# => AddInt;
               when 16#21# => SubInt;
               when 16#22# => MulInt;
               when 16#23# => DivInt;
               when 16#24# => ModUInt;
               when 16#F0# => JumpUncond;
               when others => null;
            end case;
            f.PC := f.PC + 1;
            rca := f.PC in f.code'Range;
         end loop;
         if not rcb then
            f.res := FcFailure;
         end if;
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
      pm : PtrModule;
   begin

      pm := new Module(128, 1, 1);

      pf := new PhiFunction(8);
      pf.frame := new LocalData(128, 16, false);
      pf.frame.sData(0).w := 0;
      pf.frame.sData(1).w := 0;
      pf.frame.upLink := pm.data'Access;
      pm.func(0) := pf;

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
      pf.PC := pf.code'First;

      M := testInstructions / Integer(pf.cs);
      tb := Ada.Real_Time.Clock;
      for i in 1 .. M loop
         call(pf, pm);
      end loop;
      te := Ada.Real_Time.Clock;

      Put("Duration: ");
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put("Result: ");
      Put_Line(Word32'Image(pf.frame.sData(0).w));
   end DoTest;

end bvmkp;
