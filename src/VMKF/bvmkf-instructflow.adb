package body bvmkf.InstructFlow is

   procedure impl_opcode(ins : in out Jump) is
   begin
      ins.func.PC := ins.func.PC + ins.offset;
   end impl_opcode;

   procedure impl_opcode(ins : in out JumpCondW) is
   begin
      if ins.p.all /= 0 then
         ins.func.PC := ins.func.PC + ins.offset;
      end if;
   end impl_opcode;

   procedure impl_opcode(ins : in out JumpCondB) is
      wx : W32Bits;
   begin
      wx.w := ins.p.all;
      if wx.x(ins.x) then
         ins.func.PC := ins.func.PC + ins.offset;
      end if;
   end impl_opcode;

   procedure impl_opcode(ins : in out FuncCall) is
   begin
      call(ins.extFunc.all);
      -- TODO: may be to set some boolean flag to inform that
      -- the call has been finished with Failure or not.
      -- [ins.extFunc.res]
   end impl_opcode;

   procedure impl_opcode(ins : in out FuncParam) is
   begin
      -- I void to check the extFunc and paramID because
      -- this can be done in a binding stage
      -- ASSERT(ins.id < ins.extFunc.frame.N)
      ins.extFunc.frame.sData.m(ins.id) := ins.arg.all;
   end impl_opcode;


   procedure impl_opcode(ins : in out FuncReturn) is
   begin
      ins.func.res := FcExit;
   end impl_opcode;

end bvmkf.InstructFlow;
