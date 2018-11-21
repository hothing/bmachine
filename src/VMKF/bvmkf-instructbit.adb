package body bvmkf.InstructBit is

   -- [Bit instructions]

   procedure impl_opcode(ins : in out SetBit) is
      wx : W32Bits;
   begin
      wx.w := ins.p1.all;
      wx.x(ins.x1) := true;
      ins.p1.all := wx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out ResetBit) is
      wx : W32Bits;
   begin
      wx.w := ins.p1.all;
      wx.x(ins.x1) := false;
      ins.p1.all := wx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out InvertBit)  is
      wx : W32Bits;
   begin
      wx.w := ins.p1.all;
      wx.x(ins.x1) := not wx.x(ins.x1);
      ins.p1.all := wx.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out AndBit) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      bx3.x(ins.x3) := bx1.x(ins.x1) and bx2.x(ins.x2);
      ins.p3.all := bx3.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out AndNotBit) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      bx3.x(ins.x3) := bx1.x(ins.x1) and not bx2.x(ins.x2);
      ins.p3.all := bx3.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out OrBit) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      bx3.x(ins.x3) := bx1.x(ins.x1) or bx2.x(ins.x2);
      ins.p3.all := bx3.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out OrNotBit) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      bx3.x(ins.x3) := bx1.x(ins.x1) or not bx2.x(ins.x2);
      ins.p3.all := bx3.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out XorBit) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      bx3.x(ins.x3) := bx1.x(ins.x1) xor bx2.x(ins.x2);
      ins.p3.all := bx3.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out RSTrigger) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      if bx2.x(ins.x2) then
         bx3.x(ins.x3) := true;
      elsif bx1.x(ins.x1) then
         bx3.x(ins.x3) := false;
      end if;
      ins.p3.all := bx3.w;
   end impl_opcode;

   procedure impl_opcode(ins : in out SRTrigger) is
      bx1, bx2, bx3 : W32Bits;
   begin
      bx1.w := ins.p1.all;
      bx2.w := ins.p2.all;
      bx3.w := ins.p3.all;
      if bx1.x(ins.x1) then
         bx3.x(ins.x3) := true;
      elsif bx2.x(ins.x2) then
         bx3.x(ins.x3) := false;
      end if;
      ins.p3.all := bx3.w;
   end impl_opcode;

end bvmkf.InstructBit;
