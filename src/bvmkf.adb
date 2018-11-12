with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvmkf is

   type Inst_AddInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_AddInt);

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
