with Component_Tests;

package body Stardust_Engine_Tests is

   use Ahven;

   function Get_Test_Suite return Ahven.Framework.Test_Suite is
      S : Framework.Test_Suite := Framework.Create_Suite ("Stardust_Engine");

      Component_Test : Component_Tests.Test;
   begin
      Framework.Add_Static_Test (S, Component_Test);

      return S;
   end Get_Test_Suite;

end Stardust_Engine_Tests;
