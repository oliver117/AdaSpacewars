generic
   type Component_Type (<>) is new Component with private;
package Stardust_Engine.Entities.Components is
   type Cursor is private;

   function Component (Position : Cursor) return Component_Type;

   procedure Replace_Component
     (E : in out Entity;
      Position  : Cursor;
      New_Item  : Component_Type);

   procedure Query_Component
     (Position : Cursor;
      Process  : not null access procedure (Component : Component_Type));

   procedure Update_Component
     (E : in out Entity;
      Position  : Cursor;
      Process   : not null access procedure (Component : in out Component_Type));

   --

   function First (E : Entity) return Cursor;

   function First_Component (E : Entity) return Component_Type;

   function Last (E : Entity) return Cursor;

   function Last_Component (E : Entity) return Component_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   No_Element : constant Cursor;

   function Has_Component (Position : Cursor) return Boolean;

   procedure Iterate
     (E : Entity;
      Process   : not null access procedure (Position : Cursor));

   procedure Reverse_Iterate
     (E : Entity;
      Process   : not null access procedure (Position : Cursor));

   No_Such_Component : exception;
private
   type Cursor is new Component_Lists.Cursor;

   No_Element : constant Cursor := Cursor (Component_Lists.No_Element);
end Stardust_Engine.Entities.Components;
