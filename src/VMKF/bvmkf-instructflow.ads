package bvmkf.InstructFlow is

  type Jump is new Instruction with record
      func   : PtrMuFunction;
      offset : Address;
   end record;
   procedure impl_opcode(ins : in out Jump);
   
   type JumpCondW is new Jump with record
      p      : PtrWord32;
   end record;
   procedure impl_opcode(ins : in out JumpCondW);

   type JumpCondB is new JumpCondW with record
      x      : BitAddress;
   end record;
   procedure impl_opcode(ins : in out JumpCondB);
   
   type FuncCall is new Instruction with record
      extFunc  : PtrMuFunction;
   end record;
   procedure impl_opcode(ins : in out FuncCall);

   type FuncParam is new Instruction with record
      extFunc  : PtrMuFunction; -- a calling function
      arg      : PtrWord32; -- pointer to the value
      id       : Address; -- id of a function argument
   end record;
   procedure impl_opcode(ins : in out FuncParam);

   type FuncReturn is new Instruction with record
      func   : PtrMuFunction; -- a called function
   end record;
   procedure impl_opcode(ins : in out FuncReturn); 

end bvmkf.InstructFlow;
