with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with bold;
with bnew;

procedure Main is

begin
   --bold.DoTest;
   bnew.DoTest;
end Main;

-- elapsed time: 00.75s (mode:default; 1e7 instr)
-- elapsed time: 00.44s (mode:optimize; 1e7 instr)
-- elapsed time: 01.00s (mode:optimize; 1e8 instr)
-- elapsed time: 03.10s (mode:default; 1e8 instr)
-- elapsed time: 00.75s (mode:optimize; 1e8 instr)

-- this results may be good, but may improved
-- if a descriminator check will be avoided
