with Ahven;

with Stardust_Engine.Entities;
with Stardust_Engine.Entities.Components;

package body Component_Tests is

   use Ahven;
   use Stardust_Engine;

   procedure Assert_Natural_Equal is new Assert_Equal (Data_Type => Natural,
                                                       Image     => Natural'Image);

   type Component_A is new Entities.Component with record
      A : Integer;
   end record;

   type Component_B is new Entities.Component with record
      B : Integer;
   end record;

   package Access_A is new Entities.Components (Component_Type => Component_A);


   package Access_B is new Entities.Components (Component_Type => Component_B);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Test) is
      use Framework;
   begin
      Set_Name (T, "Component");

      Add_Test_Routine (T, Test_Add_Components'Access, "Add_Components");
      Add_Test_Routine (T, Test_Replace_Component'Access, "Replace_Component");
      Add_Test_Routine (T, Test_Query_Component'Access, "Query_Component");
      Add_Test_Routine (T, Test_Update_Component'Access, "Update_Component");
      Add_Test_Routine (T, Test_Loop_First_To_Last'Access, "Loop_First_To_Last");
      Add_Test_Routine (T, Test_Loop_Last_To_First'Access, "Loop_Last_To_First");
      Add_Test_Routine (T, Test_Has_Component'Access, "Test_Has_Component");
      Add_Test_Routine (T, Test_Iterate'Access, "Test_Iterate");
      Add_Test_Routine (T, Test_Reverse_Iterate'Access, "Test_Reverse_Iterate");
      Add_Test_Routine (T, Test_Component_From_First_Last'Access, "Test_Component_From_First_Last");
   end Initialize;

   -------------------------
   -- Test_Add_Components --
   -------------------------

   procedure Test_Add_Components is
      use Entities;

      A: Component_A;
      B : Component_B;

      E : Entity := New_Entity;
   begin
      A.A := 1;
      B.B := 2;

      Assert(not Has_Components (E), "Entity should not have components!");
      Assert_Natural_Equal (Count_Components (E), 0, "Entity should have 0 components!");

      Add_Component (E, A);

      Assert(Has_Components (E), "Entity should have components! (1)");
      Assert_Natural_Equal (Count_Components (E), 1, "Entity should have 1 components!");

      Add_Component (E, B);

      Assert(Has_Components (E), "Entity should have components! (2)");
      Assert_Natural_Equal (Count_Components (E), 2, "Entity should have 2 components!");
   end Test_Add_Components;

   ----------------------------
   -- Test_Replace_Component --
   ----------------------------

   procedure Test_Replace_Component is
      use Entities;

      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);

      E : Entity := New_Entity;
      C_A : Access_A.Cursor;
   begin
      Add_Component (E, A1);

      C_A := Access_A.First (E);

      Access_A.Replace_Component (E        => E,
                                  Position => C_A,
                                  New_Item => A2);

      Assert (Access_A.Component (C_A) = A2, "Component at Position C_A should be A2");

      Access_A.Replace_Component (E        => E,
                                  Position => C_A,
                                  New_Item => A1);

      Assert (Access_A.Component (C_A) = A1, "Component at Position C_A should be A1");
   end Test_Replace_Component;

   --------------------------
   -- Test_Query_Component --
   --------------------------

   procedure Test_Query_Component is
      use Entities;
      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);

      B1 : Component_B := (B => 3);
      B2 : Component_B := (B => 4);

      C_A : Access_A.Cursor;
      C_B : Access_B.Cursor;

      E : Entity := New_Entity;

      Expected : Integer;

      procedure Query_A (C : Component_A) is
      begin
         Assert (Expected = C.A, "A should have been " & Integer'Image (Expected) & " but was " & Integer'Image (C.A) & "!");
      end Query_A;

      procedure Query_B (C : Component_B) is
      begin
         Assert (Expected = C.B, "B should have been " & Integer'Image (Expected) & " but was " & Integer'Image (C.B) & "!");
      end Query_B;
   begin
      Add_Component (E, A1);
      Add_Component (E, A2);
      Add_Component (E, B1);
      Add_Component (E, B2);

      C_A := Access_A.First (E);
      C_B := Access_B.First (E);

      -- A

      Expected := A1.A;

      Access_A.Query_Component (C_A, Query_A'Access);

      Access_A.Next (C_A);

      Expected := A2.A;

      Access_A.Query_Component (C_A, Query_A'Access);

      -- B

      Expected := B1.B;

      Access_B.Query_Component (C_B, Query_B'Access);

      Access_B.Next (C_B);

      Expected := B2.B;

      Access_B.Query_Component (C_B, Query_B'Access);
   end Test_Query_Component;

   ---------------------------
   -- Test_Update_Component --
   ---------------------------

   procedure Test_Update_Component is
      use Entities;
      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 3);

      C_A : Access_A.Cursor;

      E : Entity := New_Entity;

      procedure Increment (C : in out Component_A) is
      begin
         C.A := C.A + 1;
      end Increment;
   begin
      Add_Component (E, A1);
      Add_Component (E, A2);

      C_A := Access_A.First (E);

      Assert (Access_A.Component (C_A).A = 1, "A should have been 2!");

      Access_A.Update_Component (E, C_A, Increment'Access);

      Assert (Access_A.Component (C_A).A = 2, "A should have been 2!");

      Access_A.Next (C_A);

      Assert (Access_A.Component (C_A).A = 3, "A should have been 3!");

      Access_A.Update_Component (E, C_A, Increment'Access);

      Assert (Access_A.Component (C_A).A = 4, "A should have been 4!");
   end Test_Update_Component;

   -----------------------------
   -- Test_Loop_First_To_Last --
   -----------------------------

   procedure Test_Loop_First_To_Last is
      use Entities;

      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);
      A3 : Component_A := (A => 4);

      B1 : Component_B := (B => 1);
      B2 : Component_B := (B => 2);
      B3 : Component_B := (B => 4);

      Counter_A, Counter_B : Natural := 0;

      C_A : Access_A.Cursor;
      C_B : Access_B.Cursor;

      E : Entities.Entity := New_Entity;
   begin
      Add_Component (E, A1);
      Add_Component (E, B1);
      Add_Component (E, A2);
      Add_Component (E, A3);
      Add_Component (E, B2);
      Add_Component (E, B3);

      C_A := Access_A.First (E);
      C_B := Access_B.First (E);

      while Access_A.Has_Component (C_A) loop
         Counter_A := Counter_A + Access_A.Component (C_A).A;
         Access_A.Next (C_A);
      end loop;

      Assert_Natural_Equal (Counter_A, 7, "Counter_A should be six, is " & Natural'Image (Counter_A) & "! (1)");

      Counter_A := 0;
      C_A := Access_A.First (E);

      while Access_A.Has_Component (C_A) loop
         Counter_A := Counter_A + Access_A.Component (C_A).A;
         C_A := Access_A.Next (C_A);
      end loop;

      Assert_Natural_Equal (Counter_A, 7, "Counter_A should be six, is " & Natural'Image (Counter_A) & "! (2)");

      while Access_B.Has_Component (C_B) loop
         Counter_B := Counter_B + Access_B.Component (C_B).B;
         Access_B.Next (C_B);
      end loop;

      Assert_Natural_Equal (Counter_B, 7, "Counter_B should be six, is " & Natural'Image (Counter_B) & "! (1)");

      Counter_B := 0;
      C_B := Access_B.First (E);

      while Access_B.Has_Component (C_B) loop
         Counter_B := Counter_B + Access_B.Component (C_B).B;
         C_B := Access_B.Next (C_B);
      end loop;

      Assert_Natural_Equal (Counter_B, 7, "Counter_B should be six, is " & Natural'Image (Counter_B) & "! (2)");
   end Test_Loop_First_To_Last;

   -----------------------------
   -- Test_Loop_Last_To_First --
   -----------------------------

   procedure Test_Loop_Last_To_First is
      use Entities;

      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);
      A3 : Component_A := (A => 4);

      B1 : Component_B := (B => 1);
      B2 : Component_B := (B => 2);
      B3 : Component_B := (B => 4);

      Counter_A, Counter_B : Natural := 0;

      C_A : Access_A.Cursor;
      C_B : Access_B.Cursor;

      E : Entity := New_Entity;
   begin
      Add_Component (E, A1);
      Add_Component (E, B1);
      Add_Component (E, A2);
      Add_Component (E, A3);
      Add_Component (E, B2);
      Add_Component (E, B3);

      C_A := Access_A.Last (E);
      C_B := Access_B.Last (E);

      while Access_A.Has_Component (C_A) loop
         Counter_A := Counter_A + Access_A.Component (C_A).A;
         Access_A.Previous (C_A);
      end loop;

      Assert_Natural_Equal (Counter_A, 7, "Counter_A should be six, is " & Natural'Image (Counter_A) & "! (1)");

      Counter_A := 0;
      C_A := Access_A.Last (E);

      while Access_A.Has_Component (C_A) loop
         Counter_A := Counter_A + Access_A.Component (C_A).A;
         C_A := Access_A.Previous (C_A);
      end loop;

      Assert_Natural_Equal (Counter_A, 7, "Counter_A should be six, is " & Natural'Image (Counter_A) & "! (2)");

      while Access_B.Has_Component (C_B) loop
         Counter_B := Counter_B + Access_B.Component (C_B).B;
         Access_B.Previous (C_B);
      end loop;

      Assert_Natural_Equal (Counter_B, 7, "Counter_B should be six, is " & Natural'Image (Counter_B) & "! (1)");

      Counter_B := 0;
      C_B := Access_B.Last (E);

      while Access_B.Has_Component (C_B) loop
         Counter_B := Counter_B + Access_B.Component (C_B).B;
         C_B := Access_B.Previous (C_B);
      end loop;

      Assert_Natural_Equal (Counter_B, 7, "Counter_B should be six, is " & Natural'Image (Counter_B) & "! (2)");
   end Test_Loop_Last_To_First;

   ------------------------
   -- Test_Has_Component --
   ------------------------

   procedure Test_Has_Component is
      use Entities;

      A : Component_A := (A => 1);
      B : Component_B := (B => 2);
      E : Entity := New_Entity;

      C_A : Access_A.Cursor;
   begin
      Add_Component(E, A);
      Add_Component(E, B);

      C_A := Access_A.First (E);

      Assert (Access_A.Has_Component (C_A), "Cursor should have a component!");

      Access_A.Next (C_A);

      Assert (not Access_A.Has_Component (C_A), "Cursor should not have a component!");
   end Test_Has_Component;

   --------------------------------
   -- Test_Component_From_Cursor --
   --------------------------------

   procedure Test_Component_From_First_Last is
      use Entities;

      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);

      E : Entity;

      C_A : Access_A.Cursor;
   begin
      Add_Component (E, A1);
      Add_Component (E, A2);

      C_A := Access_A.First (E);

      Assert (Access_A.Component (C_A) = A1, "Component at first position should be A1!");

      C_A := Access_A.Last (E);

      Assert (Access_A.Component (C_A) = A2, "Component at last position should be A2!");

      Assert (Access_A.First_Component (E) = A1, "First component should be A1!");

      Assert (Access_A.Last_Component (E) = A2, "Last component should be A2!");

      --

      declare
      	use type Access_B.Cursor;
      begin
         Assert (Access_B.First (E) = Access_B.No_Element, "There should be no first component of type B!");

         Assert (Access_B.Last (E) = Access_B.No_Element, "There should be no last component of type B!");
      end;
   end Test_Component_From_First_Last;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate is
      use Entities;

      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);
      A3 : Component_A := (A => 3);

      -- Counter_A will be *increased* during iteration in
      -- order to check the iteration order
      Counter_A : Natural := 0;

      E : Entity;

      procedure Iter (C : Access_A.Cursor) is
      begin
         Assert (Counter_A < Access_A.Component (C).A, "Counter_A should have been lower than " & Integer'Image (Access_A.Component (C).A) & "!");
         Counter_A := Access_A.Component (C).A;
      end Iter;
   begin
      Add_Component (E, A1);
      Add_Component (E, A2);
      Add_Component (E, A3);

      Access_A.Iterate (E, Iter'Access);
   end Test_Iterate;

   --------------------------
   -- Test_Reverse_Iterate --
   --------------------------

   procedure Test_Reverse_Iterate is
      use Entities;

      A1 : Component_A := (A => 1);
      A2 : Component_A := (A => 2);
      A3 : Component_A := (A => 3);

      -- Counter_A will be *decreased* during iteration in
      -- order to check the iteration order
      Counter_A : Natural := 4;

      E : Entity;

      procedure Iter (C : Access_A.Cursor) is
      begin
         Assert (Counter_A > Access_A.Component (C).A, "Counter_A should have been greater than " & Integer'Image (Access_A.Component (C).A) & "!");
         Counter_A := Access_A.Component (C).A;
      end Iter;
   begin
      Add_Component (E, A1);
      Add_Component (E, A2);
      Add_Component (E, A3);

      Access_A.Reverse_Iterate (E, Iter'Access);
   end Test_Reverse_Iterate;

end Component_Tests;
