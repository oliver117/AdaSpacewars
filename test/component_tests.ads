with Ahven.Framework;

package Component_Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Test);
private
   procedure Test_Add_Components;

   procedure Test_Replace_Component;
   procedure Test_Query_Component;
   procedure Test_Update_Component;

   procedure Test_Loop_First_To_Last;
   procedure Test_Loop_Last_To_First;

   procedure Test_Has_Component;
   procedure Test_Component_From_First_Last;

   procedure Test_Iterate;
   procedure Test_Reverse_Iterate;
end Component_Tests;
