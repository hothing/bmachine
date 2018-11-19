with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvmkf is

   -- [Integer instructions]

   type Inst_AddInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_AddInt);

   type Inst_SubInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_SubInt);

   type Inst_MulInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_MulInt);

   type Inst_DivInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_DivInt);

   type Inst_ModInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_ModInt);

   -- [Float32 instructions]

   type Inst_AddFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_AddFloat);

   type Inst_SubFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_SubFloat);

   type Inst_MulFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_MulFloat);

   type Inst_DivFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_DivFloat);

   -- [Convertion W32 <-> Float]

   type Inst_Word2Float is new Instruction with record
      p1, p2 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_Word2Float);

   type Inst_Round is new Instruction with record
      p1, p2 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_Round);

   -- [Bit instructions]

   type Inst_AndBit is new Instruction with record
      p1, p2, p3 : PtrWord32;
      x1, x2, x3 : BitAddress;
   end record;
   procedure impl_opcode(ins : in out Inst_AndBit);

   type Inst_AndNotBit is new Inst_AndBit with null record;
   procedure impl_opcode(ins : in out Inst_AndNotBit);

   type Inst_OrBit is new Inst_AndBit with null record;
   procedure impl_opcode(ins : in out Inst_OrBit);

   type Inst_OrNotBit is new Inst_OrBit with null record;
   procedure impl_opcode(ins : in out Inst_OrNotBit);

   type Inst_XorBit is new Inst_AndBit with null record;
   procedure impl_opcode(ins : in out Inst_XorBit);

   -- [Comparision Integer]

   type Inst_EqInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
      x3 : BitAddress;
   end record;
   procedure impl_opcode(ins : in out Inst_EqInt);

   type Inst_NeqInt is new Inst_EqInt with null record;
   procedure impl_opcode(ins : in out Inst_NeqInt);

   type Inst_GtInt is new Inst_EqInt with null record;
   procedure impl_opcode(ins : in out Inst_GtInt);

   type Inst_GeInt is new Inst_EqInt with null record;
   procedure impl_opcode(ins : in out Inst_GeInt);

   type Inst_LtInt is new Inst_EqInt with null record;
   procedure impl_opcode(ins : in out Inst_LtInt);

   type Inst_LeInt is new Inst_EqInt with null record;
   procedure impl_opcode(ins : in out Inst_LeInt);

   -- [ControlFlow + Structural access instructions]

   type Inst_GetElem is new Instruction with record
      local : PtrLocalData;
      base  : Address;
      id    : Address;
      tgt   : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_GetElem);

   type Inst_GetElemDyn is new Instruction with record
      local   : PtrLocalData;
      base    : Address;
      dyn_id  : PtrWord32;
      tgt     : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_GetElemDyn);

   type Inst_Jump is new Instruction with record
      func   : PtrMuFunction;
      offset : Address;
   end record;
   procedure impl_opcode(ins : in out Inst_Jump);

   type Inst_Call is new Instruction with record
      extFunc  : PtrMuFunction;
   end record;
   procedure impl_opcode(ins : in out Inst_Call);

   type Inst_CallParam is new Instruction with record
      extFunc  : PtrMuFunction; -- a calling function
      arg      : PtrWord32; -- pointer to the value
      id       : Address; -- id of a function argument
   end record;
   procedure impl_opcode(ins : in out Inst_CallParam);

   type Inst_Return is new Instruction with record
      func   : PtrMuFunction; -- a called function
   end record;
   procedure impl_opcode(ins : in out Inst_Return);

   --------------------------------------------------

   procedure exec(ins : in out Instruction'Class) is
   begin
      impl_opcode(ins);
   end exec;

   procedure impl_opcode(ins : in out Instruction) is
   begin
      null;
   end impl_opcode;



   -- [Integer instructions]

   procedure impl_opcode(ins : in out Inst_AddInt) is
   begin
      ins.p3.all := ins.p1.all + ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_SubInt) is
   begin
      ins.p3.all := ins.p1.all - ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_MulInt) is
   begin
      ins.p3.all := ins.p1.all * ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_DivInt) is
   begin
      ins.p3.all := ins.p1.all / ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_ModInt) is
   begin
      ins.p3.all := ins.p1.all mod ins.p2.all;
   end impl_opcode;

   -- [Float32 instructions]

   procedure impl_opcode(ins : in out Inst_AddFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) + W32ToFloat(ins.p2.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_SubFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) - W32ToFloat(ins.p2.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_MulFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) * W32ToFloat(ins.p2.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_DivFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) / W32ToFloat(ins.p2.all));
   end impl_opcode;

   -- [Convertion W32 <-> Float]

   procedure impl_opcode(ins : in out Inst_Word2Float) is
   begin
      ins.p2.all := FloatToW32(Float(ins.p1.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_Round) is
   begin
      ins.p2.all := Word32(Float'Rounding(W32ToFloat(ins.p1.all)));
   end impl_opcode;

   -- [Bit instructions]
   procedure impl_opcode(ins : in out Inst_AndBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) and bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_AndNotBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) and not bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_OrBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) or bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_OrNotBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) or not bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_XorBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) xor bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   -- [Comparision Integer]

   procedure impl_opcode(ins : in out Inst_EqInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) = Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_NeqInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) /= Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_GtInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) > Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_GeInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) >= Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_LtInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) < Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_LeInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) <= Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   -- [ControlFlow + Structural access instructions]

   procedure impl_opcode(ins : in out Inst_GetElem) is
   begin
      ins.tgt.all := ins.local.sData.m(ins.base + ins.id);
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_GetElemDyn) is
      id : Address;
   begin
      id := ins.dyn_id.all;
      ins.tgt.all := ins.local.sData.m(ins.base + id);
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_Jump) is
   begin
      ins.func.PC := ins.func.PC + ins.offset;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_Call) is
   begin
      call(ins.extFunc.all);
      -- TODO: may be to set some boolean flag to inform that
      -- the call has been finished with Failure or not.
      -- [ins.extFunc.res]
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_CallParam) is
   begin
      -- I void to check the extFunc and paramID because
      -- this can be done in a binding stage
      -- ASSERT(ins.id < ins.extFunc.frame.N)
      ins.extFunc.frame.sData.m(ins.id) := ins.arg.all;
   end impl_opcode;


   procedure impl_opcode(ins : in out Inst_Return) is
   begin
      ins.func.res := FcExit;
   end impl_opcode;
   ------

   procedure call (self : in out MuFunction) is
      pi : PtrInstruction;
   begin
      self.res := FcExec;
      for i in self.code'Range loop
         pi := self.code(i);
         -- ASSERT(pi /= null) - can be checked in a binding stage
         if pi /= null then
            exec(pi.all);
         else
            exit;
         end if;
         if self.res /= FcExec then
            exit;
         end if;
      end loop;
   end call;

   function execCode (self :  in out MuFunction) return FunctionResult is
   begin
      return FcFailure;
   end execCode;

   ------------
   -- DoTest --
   ------------

   testInstructions : constant Integer := 100_000_000;

   procedure DoTest1 is
      vd1, vd2 : Word32;
      tb, te : Ada.Real_Time.Time;
   begin
      tb := Ada.Real_Time.Clock;
      vd1 := 0;
      vd2 := 1;
      for i in 1 .. testInstructions loop
         vd2 := Word32(i mod 2);
         vd1 := vd1 + vd2;
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(vd1));
   end DoTest1;

   procedure DoTest2 is
      pm   : PtrModule;
      pf, pf2   : PtrMuFunction;
      n, m : Integer;
      tb, te : Ada.Real_Time.Time;
   begin

      pm  := new Module(1023, 4, 4);
      pf  := new MuFunction(16, 120, 4);
      pf2 := new MuFunction(16, 120, 4);

      pf2.frame.upLink := pf.frame'Access;

      pf.frame.sData.m(0) := 0;
      pf.frame.sData.m(1) := 1;
      pf.frame.sData.m(2) := 0;
      pf.frame.sData.m(3) := 0;
      pf.frame.sData.m(4) := 0;

      pf.code(0) := new Inst_AddInt'(p1 => pf.frame.sData.m(0)'Access,
                                     p2 => pf.frame.sData.m(1)'Access,
                                     p3 => pf.frame.sData.m(0)'Access
                                    );

      pf.code(1) := new Inst_GetElem'( local => pf.frame'Access,
                                       base => 10,
                                       id => 2,
                                       tgt => pf.frame.sData.m(5)'Access
                                    );
      pf.code(2) := new Inst_GetElemDyn'( local => pf.frame'Access,
                                       base => 10,
                                       dyn_id => pf.frame.sData.m(3)'Access,
                                       tgt => pf.frame.sData.m(4)'Access
                                      );

      N := 3;
      M := testInstructions / N;
      tb := Ada.Real_Time.Clock;
      for i in 1 .. M loop
         --exec(pi.all);
         call(pf.all);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(pf.frame.sData.m(0)));
   end DoTest2;

   procedure DoTest3 is
      vb : Value(W8);
      vl : Value(W32);
      vc : Cell(False);
      pc : Cell(True);
   begin
      Put_Line("Data formats sizes");
      Put_Line(Integer'Image(Instruction'Size / Byte'Size));
      Put_Line(Integer'Image(vb'Size / Byte'Size));
      Put_Line(Integer'Image(vl'Size / Byte'Size));
      Put_Line(Integer'Image(vc'Size / Byte'Size));
      Put_Line(Integer'Image(pc'Size / Byte'Size));
   end DoTest3;

   procedure DoTest is
   begin
      DoTest1;
      DoTest2;
      DoTest3;
   end DoTest;

end bvmkf;
