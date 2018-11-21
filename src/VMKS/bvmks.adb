with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvmks is

   type Inst_AddInt is new Instruction with record
      p1, p2, p3 : Reference;
   end record;
   procedure impl_opcode(ins : in out Inst_AddInt);

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
      arg      : Reference; -- pointer to the value
      id       : Address; -- id of a function argument
   end record;
   procedure impl_opcode(ins : in out Inst_CallParam);

   type Inst_Return is new Instruction with record
      func   : PtrMuFunction; -- a called function
   end record;
   procedure impl_opcode(ins : in out Inst_Return);

   procedure exec(ins : in out Instruction'Class) is
   begin
      impl_opcode(ins);
   end exec;

   procedure impl_opcode(ins : in out Instruction) is
   begin
      null;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_AddInt) is
   begin
      ins.p3.frame.all.sData(ins.p3.offset).w :=
        IntToW32(
                 W32ToInt(ins.p1.frame.all.sData(ins.p1.offset).w)
                 +
                   W32ToInt(ins.p2.frame.all.sData(ins.p2.offset).w)
                );
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
      ins.extFunc.frame.sData(ins.id) :=
        ins.arg.frame.all.sData(ins.arg.offset);
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
      pf   : PtrMuFunction;
      m : Integer;
      n : Word32;
      tb, te : Ada.Real_Time.Time;
   begin
      pm  := new Module(1023, 4, 4);

      pm.data.sData(0).w := 0;
      pm.data.sData(1).w := 1;

      pf  := new MuFunction(16);
      pf.frame := new LocalData(16, 0, False);
      pf.frame.upLink := pm.data'Access;

      for i in pf.frame.all.sData'Range loop
         pf.frame.all.sData(i).w := 0;
      end loop;

      n := 0;
      pf.code(n) := new Inst_AddInt'(p1 => Reference'( frame => pf.frame,
                                               offset => 0, size => 1),
                             p2 => Reference'( frame => pf.frame,
                                               offset => 1, size => 1),
                             p3 => Reference'( frame => pm.data'Access,
                                               offset => 0, size => 1));

      M := testInstructions / Integer(n + 1);
      tb := Ada.Real_Time.Clock;
      for i in 1 .. M loop
         call(pf.all);
      end loop;
      te := Ada.Real_Time.Clock;

      Put("Duration: ");
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put("Result: ");
      Put_Line(Word32'Image(pm.data.sData(0).w));
   end DoTest2;

   procedure DoTest is
   begin
      DoTest1;
      DoTest2;
      Put_Line(Integer'Image(Instruction'Size / Byte'Size));
   end DoTest;

end bvmks;