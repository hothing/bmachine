package bvmkf.InstructComp is

  -- [Comparision Integer]
   
   type EqInt is new Instruction with record
      p1, p2, p3 : PtrWord32;
      x3 : BitAddress;
   end record;
   procedure impl_opcode(ins : in out EqInt);
   
   type NeqInt is new EqInt with null record;
   procedure impl_opcode(ins : in out NeqInt);

   type GtInt is new EqInt with null record;
   procedure impl_opcode(ins : in out GtInt);
   
   type GeInt is new EqInt with null record;
   procedure impl_opcode(ins : in out GeInt);
   
   type LtInt is new EqInt with null record;
   procedure impl_opcode(ins : in out LtInt);
   
   type LeInt is new EqInt with null record;
   procedure impl_opcode(ins : in out LeInt);
   
   type EqZeroInt is new Instruction with record
      p1, p3 : PtrWord32;
      x3 : BitAddress;
   end record;
   procedure impl_opcode(ins : in out EqZeroInt);
   
   type NeqZeroInt is new EqZeroInt with null record;
   procedure impl_opcode(ins : in out NeqZeroInt); 

end bvmkf.InstructComp;
