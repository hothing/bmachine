with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with bvmks;
with bvmkf;
with bvmkp;

procedure Main is

begin
   --Put_Line("VM-KS");
   --bvmks.DoTest;
   --Put_Line("VM-KF");
   --bvmkf.DoTest;
   Put_Line("VM-KP");
   bvmkp.DoTest;
end Main;

-- elapsed time: 00.75s (mode:default; 1e7 instr)
-- elapsed time: 00.44s (mode:optimize; 1e7 instr)
-- elapsed time: 01.00s (mode:optimize; 1e8 instr)
-- elapsed time: 03.10s (mode:default; 1e8 instr)
-- elapsed time: 00.75s (mode:optimize; 1e8 instr)
-- [bvm1.DoTest] elapsed time: 00.75s (mode:default; 1e8 instr)
-- [bvm1.DoTest] elapsed time: 00.21s (mode:optimize; 1e8 instr)
-- [bvm1.DoTest2] elapsed time: 02.17s (mode:default; 1e8 instr)
-- [bvm1.DoTest2] elapsed time: 00.35s (mode:optimize; 1e8 instr)
