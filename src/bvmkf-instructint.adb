package body bvmkf.InstructInt is

   procedure impl_opcode(ins : in out AddInt) is
   begin
      ins.p3.all := ins.p1.all + ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out SubInt) is
   begin
      ins.p3.all := ins.p1.all - ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out MulInt) is
   begin
      ins.p3.all := ins.p1.all * ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out DivInt) is
   begin
      ins.p3.all := ins.p1.all / ins.p2.all;
   end impl_opcode;

   procedure impl_opcode(ins : in out ModInt) is
   begin
      ins.p3.all := ins.p1.all mod ins.p2.all;
   end impl_opcode;

end bvmkf.InstructInt;
