with Ada.Assertions;

package body Stardust_Engine.Entities.Components is

   package CL renames Component_Lists;

   ---------------
   -- Component --
   ---------------

   function Component (Position : Cursor) return Component_Type is
      C : constant Stardust_Engine.Entities.Component'Class :=
        CL.Element (CL.Cursor (Position));
   begin
      Ada.Assertions.Assert (C in Component_Type, "Component should be of type Component_Type");
      return Component_Type (C);
   end Component;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Component
     (E        : in out Entity;
      Position : Cursor;
      New_Item : Component_Type)
   is
   begin
      CL.Replace_Element (Container => E.Components,
                                       Position  => CL.Cursor (Position),
                                       New_Item  => New_Item);
   end Replace_Component;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Component
     (Position : Cursor;
      Process  : not null access procedure (Component : Component_Type))
   is
      procedure Proc (C : Stardust_Engine.Entities.Component'Class) is
      begin
         Ada.Assertions.Assert (C in Component_Type, "Queried component should be of type Component_Type");
         Process (Component_Type (C));
      end Proc;
   begin
      CL.Query_Element (Position => CL.Cursor (Position),
                        Process  => Proc'Access);
   end Query_Component;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Component
     (E        : in out Entity;
      Position : Cursor;
      Process  : not null access procedure
        (Component : in out Component_Type))
   is
      procedure Proc (C : in out Stardust_Engine.Entities.Component'Class) is
      begin
         Ada.Assertions.Assert (C in Component_Type, "Updated component should be of type Component_Type");
         Process (Component_Type (C));
      end Proc;
   begin
      CL.Update_Element (Container => E.Components,
                         Position  => CL.Cursor (Position),
                         Process   => Proc'Access);
   end Update_Component;

   -----------
   -- First --
   -----------

   function First (E : Entity) return Cursor is
      C : CL.Cursor := E.Components.First;
   begin
      while CL.Has_Element (C) loop
         if CL.Element (C) in Component_Type then
            return Cursor (C);
         end if;

         CL.Next (C);
      end loop;

      return No_Element;
   end First;

   ---------------------
   -- First_Component --
   ---------------------

   function First_Component (E : Entity) return Component_Type is
   begin
      return Component (First (E));
   end First_Component;

   ----------
   -- Last --
   ----------

   function Last (E : Entity) return Cursor is
      C : CL.Cursor := E.Components.Last;
   begin
      while CL.Has_Element (C) loop
         if CL.Element (C) in Component_Type then
            return Cursor (C);
         end if;

         CL.Previous (C);
      end loop;

      return No_Element;
   end Last;

   ------------------
   -- Last_Component --
   ------------------

   function Last_Component (E : Entity) return Component_Type is
   begin
      return Component (Last (E));
   end Last_Component;

   ----------
   -- Next --
   ----------

   function Next (Position : Cursor) return Cursor is
      C : CL.Cursor := CL.Cursor (Position);
   begin
      CL.Next (C);

      while CL.Has_Element (C) loop
         if CL.Element (C) in Component_Type then
            return Cursor (C);
         end if;

         CL.Next (C);
      end loop;

      return No_Element;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Position : Cursor) return Cursor is
      C : CL.Cursor := CL.Cursor (Position);
   begin
      CL.Previous (C);

      while CL.Has_Element (C) loop
         if CL.Element (C) in Component_Type then
            return Cursor (C);
         end if;

         CL.Previous (C);
      end loop;

      return No_Element;
   end Previous;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   -------------------
   -- Has_Component --
   -------------------

   function Has_Component (Position : Cursor) return Boolean is
      C : constant CL.Cursor := CL.Cursor (Position);
   begin
      return CL.Has_Element (C) and then (CL.Element (C) in Component_Type);
   end Has_Component;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (E       : Entity;
      Process : not null access procedure (Position : Cursor))
   is
      C : Cursor := First (E);
   begin
      while Has_Component (C) loop
         Process (C);
         Next (C);
      end loop;
   end Iterate;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (E       : Entity;
      Process : not null access procedure (Position : Cursor))
   is
      C : Cursor := Last (E);
   begin
      while Has_Component (C) loop
         Process (C);
         Previous (C);
      end loop;
   end Reverse_Iterate;

end Stardust_Engine.Entities.Components;
