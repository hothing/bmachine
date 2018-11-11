with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvmkf is

   type Inst_AddInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
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
      arg      : PtrWord32; -- pointer to the value
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
      ins.p3.all := ins.p1.all + ins.p2.all;
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
         exec(pi.all);
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

   procedure DoTest1 is
      vd1, vd2 : Word32;
      tb, te : Ada.Real_Time.Time;
   begin
      tb := Ada.Real_Time.Clock;
      vd1 := 0;
      vd2 := 1;
      for i in 1 .. 100_000_000 loop
         vd2 := Word32(i mod 2);
         vd1 := vd1 + vd2;
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(vd1));
   end DoTest1;

   procedure DoTest2 is
      pcp, ps  : PtrMemSegment;
      pm   : PtrModule;
      pi   : PtrInstruction;
      pf, pf2   : PtrMuFunction;

      tb, te : Ada.Real_Time.Time;
   begin
      pcp := new MemorySegment(1023);
      pcp.all.m(0) := 0;
      pcp.all.m(1) := 1;

      pm  := new Module(1023, 4, 4);
      pm.cp := pcp;
      ps := pm.data'Access;

      pf  := new MuFunction(16, 120, 4);
      pf2 := new MuFunction(16, 120, 4);

      pf.frame.gData := pm;
      pf2.frame.gData := pm;
      pf2.frame.upLink := pf.frame'Access;

      pi := new Inst_AddInt'(p1 => pcp.all.m(0)'Access,
                             p2 => pcp.all.m(1)'Access,
                             p3 => pcp.all.m(0)'Access
                            );

      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         exec(pi.all);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(pcp.all.m(0)));
   end DoTest2;

   procedure DoTest is
   begin
      DoTest1;
      DoTest2;
      Put_Line(Integer'Image(Instruction'Size / Byte'Size));
   end DoTest;

end bvmkf;
