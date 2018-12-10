with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with bvmks;
with bvmkr;

procedure Main is

begin
   Put_Line("VM-KS");
   bvmks.DoTest;
   Put_Line("VM-KR");
   bvmkr.DoTest;
end Main;
