with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body bvmkp is

   ----------
   -- call --
   ----------

   procedure call (func : PtrPhiFunction; fumod : PtrModule) is
      f : PtrPhiFunction;
      c : PhiCode;
      s : PtrLocalData;

      -- Flags of execution
      rca, rcb : Boolean;

      -- Ariphmetic stack
      accu   : MemoryBlock(1 .. 32);
      atop   : Address := 0;
      rlo    : Boolean; -- Result Of Logical Operation

      -- GoBack addresses stack
      type ADM is array (Address range 1 .. 8) of Address;

      gbs   : ADM;
      gtop  : Address := 0;

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

      procedure Drop2 is
      begin
         if atop > accu'First then
            accu(atop).w := 0;
            atop := atop - 1;
            accu(atop).w := 0;
            atop := atop - 1;
         end if;
      end Drop2;

      procedure Swap is
         w : Word32;
      begin
         if atop > accu'First and atop <= accu'Last then
            w := accu(atop).w;
            accu(atop).w := accu(atop - 1).w;
            accu(atop - 1).w := w;
         else
            rcb := false;
         end if;
      end Swap;

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

      procedure Pop is
      begin
         if atop in accu'Range then
            s.sData(Word32(c.sel)).w := accu(atop).w;
            atop := atop - 1;
         end if;
      end Pop;

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
               null;
            end if;
         else
            rcb := false;
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

      procedure CmpEqInt is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToInt(accu(atop).w) = W32ToInt(accu(atop - 1).w);
            --atop := atop - 2;
         else
            rcb := false;
         end if;
      end CmpEqInt;

      procedure CmpGtInt is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToInt(accu(atop).w) > W32ToInt(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpGtInt;

      procedure CmpLtInt is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToInt(accu(atop).w) < W32ToInt(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpLtInt;

      procedure CmpGeInt is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToInt(accu(atop).w) >= W32ToInt(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpGeInt;

      procedure CmpLeInt is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToInt(accu(atop).w) <= W32ToInt(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpLeInt;


      -- Floating-point instructions

      procedure AddF32 is
      begin
         if atop > accu'First then
            atop := atop - 1;
            accu(atop).w :=
              FloatToW32(
                       W32ToFloat(accu(atop + 1).w)
                       +
                         W32ToFloat(accu(atop).w)
                      );

         else
            rcb := false;
         end if;
      end AddF32;

      procedure SubF32 is
      begin
         if atop > accu'First then
            atop := atop - 1;
            accu(atop).w :=
              FloatToW32(
                       W32ToFloat(accu(atop + 1).w)
                       -
                         W32ToFloat(accu(atop).w)
                      );

         else
            rcb := false;
         end if;
      end SubF32;

      procedure MulF32 is
      begin
         if atop > accu'First then
            atop := atop - 1;
            accu(atop).w :=
              FloatToW32(
                       W32ToFloat(accu(atop + 1).w)
                       *
                         W32ToFloat(accu(atop).w)
                      );

         else
            rcb := false;
         end if;
      end MulF32;

      procedure DivF32 is
      begin
         if atop > accu'First then
            atop := atop - 1;
            if W32ToFloat(accu(atop).w) /= 0.0 then
               accu(atop).w :=
                 FloatToW32(
                            W32ToFloat(accu(atop + 1).w)
                            /
                              W32ToFloat(accu(atop).w)
                           );
            else
               null;
               -- Somehow we should inform a program about an exception
               --  rcb := false;
            end if;
         else
            rcb := false;
         end if;
      end DivF32;

      procedure PushF32_Zero is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := FloatToW32(0.0);
         else
            rcb := false;
         end if;
      end PushF32_Zero;

      procedure PushF32_One is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := FloatToW32(1.0);
         else
            rcb := false;
         end if;
      end PushF32_One;

      procedure PushF32_E is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := FloatToW32(2.78);
         else
            rcb := false;
         end if;
      end PushF32_E;

      procedure PushF32_Pi is
      begin
         if atop < accu'Last then
            atop := atop + 1;
            accu(atop).w := FloatToW32(3.14159);
         else
            rcb := false;
         end if;
      end PushF32_Pi;

      procedure SinF32 is
      begin
         if atop in accu'Range then
            accu(atop).w := FloatToW32(Sin(W32ToFloat(accu(atop).w)));
         else
            rcb := false;
         end if;
      end SinF32;

      procedure CosF32 is
      begin
         if atop in accu'Range then
            accu(atop).w := FloatToW32(Cos(W32ToFloat(accu(atop).w)));
         else
            rcb := false;
         end if;
      end CosF32;

      procedure TanF32 is
      begin
         if atop in accu'Range then
            accu(atop).w := FloatToW32(Tan(W32ToFloat(accu(atop).w)));
         else
            rcb := false;
         end if;
      end TanF32;

      procedure CotF32 is
      begin
         if atop in accu'Range then
            accu(atop).w := FloatToW32(Cot(W32ToFloat(accu(atop).w)));
         else
            rcb := false;
         end if;
      end CotF32;

      procedure LnF32 is
      begin
         if atop in accu'Range then
            accu(atop).w := FloatToW32(Log(W32ToFloat(accu(atop).w)));
         else
            rcb := false;
         end if;
      end LnF32;

      procedure LogF32 is
      begin
         if atop > accu'First and atop <= accu'Last then
            atop := atop - 1;
            accu(atop).w := FloatToW32(Log(W32ToFloat(accu(atop).w), W32ToFloat(accu(atop + 1).w)));
         else
            rcb := false;
         end if;
      end LogF32;

      procedure CmpEqF32 is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToFloat(accu(atop).w) = W32ToFloat(accu(atop - 1).w);
            --atop := atop - 2;
         else
            rcb := false;
         end if;
      end CmpEqF32;

      procedure CmpGtF32 is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToFloat(accu(atop).w) > W32ToFloat(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpGtF32;

      procedure CmpLtF32 is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToFloat(accu(atop).w) < W32ToFloat(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpLtF32;

      procedure CmpGeF32 is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToFloat(accu(atop).w) >= W32ToFloat(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpGeF32;

      procedure CmpLeF32 is
      begin
         if atop > accu'First and atop <= accu'Last then
            rlo := W32ToFloat(accu(atop).w) <= W32ToFloat(accu(atop - 1).w);
         else
            rcb := false;
         end if;
      end CmpLeF32;

      -- Convertion instructuons

      procedure CnvF2I is
         f : Float;
      begin
         if atop in accu'Range then
            f := Float'Rounding(W32ToFloat(accu(atop).w));
            accu(atop).w := IntToW32(Integer(f));
         end if;
      end CnvF2I;

      procedure CnvI2F is
      begin
         if atop in accu'Range then
            accu(atop).w := FloatToW32(Float(W32ToInt(accu(atop).w)));
         end if;
      end CnvI2F;

      -- RLO operations

      procedure SetRLO is
      begin
         rlo := true;
      end SetRLO;

      procedure ResetRLO is
      begin
         rlo := false;
      end ResetRLO;

      procedure InvertRLO is
      begin
         rlo := not rlo;
      end InvertRLO;


      -- Context change instructions

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

      procedure JumpTrue is
      begin
         if rlo then
            JumpUncond;
         end if;
      end JumpTrue;

      procedure JumpFalse is
      begin
         if not rlo then
            JumpUncond;
         end if;
      end JumpFalse;

      procedure EnterLoop is
      begin
         if gtop < gbs'Last then
            gtop := gtop + 1;
            gbs(gtop) := f.PC + 1; -- do the loop from the next instr.
            -- BUG: if this instruction will be last in a code block
            -- the top value of stack became invalid
         else
            rcb := False;
         end if;
      end EnterLoop;

      procedure NextLoop is
      begin
         --if gtop in gbs'Range then
            f.PC := gbs(gtop);
         --end if;
      end NextLoop;

      procedure ExitLoop is
      begin
         if gtop >= gbs'First then
            gtop := gtop - 1;
         end if;
      end ExitLoop;

      procedure CountLoop is
      begin
         if W32ToInt(s.sData(Word32(c.sel)).w) > 0 then
            s.sData(Word32(c.sel)).w :=
              IntToW32(W32ToInt(s.sData(Word32(c.sel)).w) - 1);
            NextLoop;
         else
            ExitLoop;
         end if;
      end CountLoop;

      procedure ForLoop is
      begin
         if W32ToInt(s.sData(Word32(c.sel)).w) >= W32ToInt(accu(atop).w) then
            accu(atop).w := IntToW32(W32ToInt(accu(atop).w) + 1);
            NextLoop;
         else
            ExitLoop;
         end if;
      end ForLoop;

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
               when 16#07# => Drop2;
               when 16#10# => Load;
               when 16#11# => Pop;
               when 16#12# => Store;
               when 16#14# => UseLocal;
               when 16#15# => UseUplevel(c.reg1);

               when 16#20# => AddInt;
               when 16#21# => SubInt;
               when 16#22# => MulInt;
               when 16#23# => DivInt;
               when 16#24# => ModUInt;
               when 16#2E# => IncInt;
               when 16#2F# => DecInt;

               when 16#30# => AddF32;
               when 16#31# => SubF32;
               when 16#32# => MulF32;
               when 16#33# => DivF32;
               when 16#34# => PushF32_Zero;
               when 16#35# => PushF32_One;
               when 16#36# => PushF32_Pi;
               when 16#37# => PushF32_E;
               when 16#40# => SinF32;
               when 16#41# => CosF32;
               when 16#42# => TanF32;
               when 16#43# => CotF32;
               when 16#44# => LnF32;
               when 16#45# => LogF32;

               when 16#90# => CnvI2F;
               when 16#91# => CnvF2I;

               when 16#A0# => CmpEqInt;
               when 16#A1# => CmpGtInt;
               when 16#A2# => CmpLtInt;
               when 16#A3# => CmpGeInt;
               when 16#A4# => CmpLeInt;
               when 16#A5# => CmpEqF32;
               when 16#A6# => CmpGtF32;
               when 16#A7# => CmpLtF32;
               when 16#A8# => CmpGeF32;
               when 16#A9# => CmpLeF32;

               when 16#D0# => SetRLO;
               when 16#D1# => ResetRLO;
               when 16#D2# => InvertRLO;

               when 16#E0# => JumpUncond;
               when 16#E1# => JumpTrue;
               when 16#E2# => JumpFalse;

               when 16#E3# => EnterLoop;
               when 16#E4# => NextLoop;
               when 16#E5# => ExitLoop;

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
         pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#02#, sel => 0);
         i := i + 1;
         while i < pf.code'Last - 2 loop
            -- PUSH 1
            pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#03#, sel => 0);
            i := i + 1;
            -- ADDI
            pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#20#, sel => 0);
            i := i + 1;
         end loop;
         -- UL
         pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#14#, sel => 0);
         i := i + 1;
         -- STORE
         pf.code(i) := PhiCode'(b => PCF_STACK, code => 16#12#, sel => 0);
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
