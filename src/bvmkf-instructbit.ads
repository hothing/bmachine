package bvmkf.InstructBit is

  -- [Bit instructions]
   
   type AndBit is new Instruction with record
      p1, p2, p3 : PtrWord32;
      x1, x2, x3 : BitAddress;
   end record;
   procedure impl_opcode(ins : in out AndBit);
   
   type AndNotBit is new AndBit with null record;
   procedure impl_opcode(ins : in out AndNotBit);
   
   type OrBit is new AndBit with null record;
   procedure impl_opcode(ins : in out OrBit);
   
   type OrNotBit is new OrBit with null record;
   procedure impl_opcode(ins : in out OrNotBit);
   
   type XorBit is new AndBit with null record;
   procedure impl_opcode(ins : in out XorBit); 

end bvmkf.InstructBit;
