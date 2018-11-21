package bvmkf.InstructFloat is

  -- [Float32 instructions]
   
   type AddFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out AddFloat);
   
   type SubFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out SubFloat);
   
   type MulFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out MulFloat);
   
   type DivFloat is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out DivFloat);
   
   -- [Convertion W32 <-> Float]
   
   type Word2Float is new Instruction with record
      p1, p2 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Word2Float);
   
   type Round is new Instruction with record
      p1, p2 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out Round);

end bvmkf.InstructFloat;
