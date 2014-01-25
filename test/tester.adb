with Ahven.Framework;
with Ahven.Text_Runner;
with Stardust_Engine_Tests;

procedure Tester is
   Suite : Ahven.Framework.Test_Suite := Stardust_Engine_Tests.Get_Test_Suite;
begin
   Ahven.Text_Runner.Run (Suite);
end Tester;
