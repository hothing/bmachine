package body bvmkf.InstructBit is

   -- [Bit instructions]
   procedure impl_opcode(ins : in out AndBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) and bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out AndNotBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) and not bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out OrBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) or bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out OrNotBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) or not bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out XorBit) is
      bx1, bx2 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx1.x(ins.x3) := bx1.x(ins.x1) xor bx2.x(ins.x2);
      ins.p3.all := bx1.w;
   end impl_opcode;

end bvmkf.InstructBit;
