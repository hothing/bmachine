with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body bvk is

   type Inst_AddInt is new Instruction with null record;
   procedure impl_opcode(ins : in out Inst_AddInt);

   type Inst2_AddInt is new Instruction2 with null record;
   procedure impl_opcode(ins : in out Inst2_AddInt);

   function call (self : in out MuFunction) return FunctionResult is
   begin
      return Failure;
   end call;

   function execCode (self :  in out MuFunction) return FunctionResult is
   begin
      return Failure;
   end execCode;


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

   procedure exec(ins : in out Instruction2'Class) is
   begin
      impl_opcode(ins);
   end exec;

   procedure impl_opcode(ins : in out Instruction2) is
   begin
      null;
   end impl_opcode;

   procedure impl_opcode(ins : in out Inst2_AddInt) is
   begin
      ins.fx.frame.sData(ins.r3) := ins.fx.frame.sData(ins.r1) + ins.fx.frame.sData(ins.r2);
   end impl_opcode;

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

      pi := new Inst_AddInt;
      pi.p1 := pcp.all(0)'Access;
      pi.p2 := pcp.all(1)'Access;
      pi.p3 := pcp.all(0)'Access;

      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         exec(pi.all);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(pcp.all(0)));
   end DoTest2;

   procedure DoTest3 is
      pld  : PtrLocalData;
      pi   : PtrInstruction2;

      tb, te : Ada.Real_Time.Time;
   begin

      pld := new LocalData(128, 8);

      pld.all.sData(0) := 0;
      pld.all.sData(1) := 1;

      pi := new Inst2_AddInt;
      pi.r1 := 0;
      pi.r2 := 1;
      pi.r3 := 0;
      pi.fx := new MuFunction(16);
      pi.fx.frame := pld;

      tb := Ada.Real_Time.Clock;
      for i in 1 .. 100_000_000 loop
         exec(pi.all);
      end loop;
      te := Ada.Real_Time.Clock;
      Put_Line(Duration'Image(To_Duration(te - tb)));
      Put_Line(Word32'Image(pld.all.sData(0)));
   end DoTest3;

   procedure DoTest is
   begin
      DoTest1;
      DoTest2;
      DoTest3;
      Put_Line(Integer'Image(Instruction'Size / Byte'Size));
      Put_Line(Integer'Image(Instruction2'Size / Byte'Size));
   end DoTest;

end bvk;
