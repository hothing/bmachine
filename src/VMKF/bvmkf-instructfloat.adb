package body bvmkf.InstructFloat is

   -- [Float32 instructions]

   procedure impl_opcode(ins : in out AddFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) + W32ToFloat(ins.p2.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out SubFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) - W32ToFloat(ins.p2.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out MulFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) * W32ToFloat(ins.p2.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out DivFloat) is
   begin
      ins.p3.all := FloatToW32(W32ToFloat(ins.p1.all) / W32ToFloat(ins.p2.all));
   end impl_opcode;

   -- [Convertion W32 <-> Float]

   procedure impl_opcode(ins : in out Word2Float) is
   begin
      ins.p2.all := FloatToW32(Float(ins.p1.all));
   end impl_opcode;

   procedure impl_opcode(ins : in out Round) is
   begin
      ins.p2.all := Word32(Float'Rounding(W32ToFloat(ins.p1.all)));
   end impl_opcode;

end bvmkf.InstructFloat;
