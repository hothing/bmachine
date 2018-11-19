package body bvmkf.InstructComp is

   -- [Comparision Integer]

   procedure impl_opcode(ins : in out EqInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) = Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out NeqInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) /= Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out GtInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) > Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out GeInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) >= Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out LtInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) < Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out LeInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) <= Integer(ins.p2.all));
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out EqZeroInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) = 0);
      ins.p3.all := bx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out NeqZeroInt) is
      bx : W32Bits;
   begin
      bx.x(ins.x3) := Bit(Integer(ins.p1.all) /= 0);
      ins.p3.all := bx.w;
   end impl_opcode;


end bvmkf.InstructComp;
