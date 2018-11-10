with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvk is

   type Inst_AddInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Inst_AddInt);

   type Inst_Jump is new Instruction with record
      offset : Address;
      func   : PtrMuFunction;
   end record;
   procedure impl_opcode(ins : in out Inst_Jump);

   type Inst_Call is new Instruction with record
      extFunc   : PtrMuFunction;
   end record;
   procedure impl_opcode(ins : in out Inst_Call);

   type Inst_CallParam is new Instruction with record
      extFunc  : PtrMuFunction; -- called function
      arg      : PtrWord32; -- pointer to the value
      id       : Address; -- id of a function argument
   end record;
   procedure impl_opcode(ins : in out Inst_CallParam);

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
      ins.p3.all := ins.p1.all +ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_Jump) is
   begin
      ins.func.PC := ins.func.PC + ins.offset;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_Call) is
   begin
      if ins.extFunc /= null then
         call(ins.extFunc.all);
      end if;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst_CallParam) is
   begin
      if ins.extFunc /= null then
         if ins.extFunc.frame /= null then
            -- how to avoid this^ check??
            ins.extFunc.frame.sData(ins.id) := ins.arg.all;
         end if;
      end if;
   end impl_opcode;
   ------

   procedure call (self : in out MuFunction) is
      pi : PtrInstruction;
   begin
      for i in self.code'Range loop
         pi := self.code(i);
         if pi /= null then
            exec(pi.all);
         else
            exit;
         end if;
      end loop;
   end call;

   function execCode (self :  in out MuFunction) return FunctionResult is
   begin
      return Failure;
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
      pld  : PtrLocalData;
      pcp  : PtrMemSegment;
      pm   : PtrModule;
      pi   : PtrInstruction;

      tb, te : Ada.Real_Time.Time;
   begin
      pcp := new MemorySegment(0 .. 1023);
      pcp.all(0) := 0;
      pcp.all(1) := 1;

      pm  := new Module(1023, 4, 4);
      pm.cp := pcp;

      pld := new LocalData(128, 8);

      pi := new Inst_AddInt'(p1 => pcp.all(0)'Access,
                             p2 => pcp.all(1)'Access,
                             p3 => pcp.all(0)'Access
                            );

      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         exec(pi.all);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(pcp.all(0)));
   end DoTest2;

   procedure DoTest is
   begin
      DoTest1;
      DoTest2;
      Put_Line(Integer'Image(Instruction'Size / Byte'Size));
   end DoTest;

end bvk;
