package bvmkf.InstructInt is

  -- [Integer instructions]
   
   type AddInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out AddInt);
   
   type SubInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out SubInt);
   
   type MulInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out MulInt);
   
   type DivInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out DivInt);
   
   type ModInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out ModInt); 

end bvmkf.InstructInt;
