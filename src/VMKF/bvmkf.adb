with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with bvmkf.InstructInt;
with bvmkf.InstructBit;

package body bvmkf is

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

   --------------------------------------------------

   -- [Common]

   procedure exec(ins : in out Instruction'Class) is
   begin
      impl_opcode(ins);
   end exec;

   procedure impl_opcode(ins : in out Instruction) is
   begin
      null;
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
      m : Integer;
      n : Word32;
      tb, te : Ada.Real_Time.Time;
   begin

      pm  := new Module(1023, 4, 4);
      pf  := new MuFunction(16, 120, 4);
      pf2 := new MuFunction(16, 120, 4);

      pf2.frame.upLink := pf.frame'Access;

      pf.frame.sData.m(0) := 1;
      pf.frame.sData.m(1) := 1;
      pf.frame.sData.m(2) := 1;
      pf.frame.sData.m(3) := 1;
      pf.frame.sData.m(4) := 1;



      n := 0;
      pf.code(n) := new Inst_GetElem'( local => pf.frame'Access,
                                       base => 10,
                                       id => 2,
                                       tgt => pf.frame.sData.m(5)'Access
                                    );
      n := n + 1;
      pf.code(n) := new Inst_GetElemDyn'( local => pf.frame'Access,
                                       base => 10,
                                       dyn_id => pf.frame.sData.m(3)'Access,
                                       tgt => pf.frame.sData.m(4)'Access
                                      );

      -- integer instr
      n := n + 1;
      pf.code(n) := new InstructInt.AddInt'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access
                                    );
      n := n + 1;
      pf.code(n) := new InstructInt.SubInt'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access
                                           );
      n := n + 1;
      pf.code(n) := new InstructInt.MulInt'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access
                                           );
      n := n + 1;
      pf.code(n) := new InstructInt.DivInt'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access
                                           );
      n := n + 1;
      pf.code(n) := new InstructInt.ModInt'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access
                                           );

      -- bit instructions
      n := n + 1;
      pf.code(n) := new InstructBit.AndBit'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                           );
      n := n + 1;
      pf.code(n) := new InstructBit.AndNotBit'(p1 => pf.frame.sData.m(0)'Access,
                                               p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                           );
      n := n + 1;
      pf.code(n) := new InstructBit.OrBit'(p1 => pf.frame.sData.m(0)'Access,
                                           p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                           );
      n := n + 1;
      pf.code(n) := new InstructBit.OrNotBit'(p1 => pf.frame.sData.m(0)'Access,
                                              p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                             );
      n := n + 1;
      pf.code(n) := new InstructBit.XorBit'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                           );

      n := n + 1;
      pf.code(n) := new InstructBit.RSTrigger'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                           );

      n := n + 1;
      pf.code(n) := new InstructBit.SRTrigger'(p1 => pf.frame.sData.m(0)'Access,
                                            p2 => pf.frame.sData.m(1)'Access,
                                            p3 => pf.frame.sData.m(0)'Access,
                                            x1 => 0,
                                            x2 => 1,
                                            x3 => 2
                                           );

      n := n + 1;
      pf.code(n) := new InstructBit.SetBit'(p1 => pf.frame.sData.m(0)'Access,
                                            x1 => 0);

      n := n + 1;
      pf.code(n) := new InstructBit.ReSetBit'(p1 => pf.frame.sData.m(0)'Access,
                                            x1 => 0);

      n := n + 1;
      pf.code(n) := new InstructBit.InvertBit'(p1 => pf.frame.sData.m(0)'Access,
                                            x1 => 0);

      ----------------------------------

      M := testInstructions / Integer(n);
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
