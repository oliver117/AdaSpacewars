with Allegro5.Bitmap_Draw;
with Allegro5.Bitmap_IO;

with Interfaces.C.Strings;

package body Stardust_Engine.Entities is

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component (E : in out Entity; C : in Component'Class) is
   begin
      E.Components.Append (C);
   end Add_Component;

   -------------------
   -- Get_Component --
   -------------------

   function Get_Component (E : in Entity; CT : in Ada.Tags.Tag) return Component'Class is
      use type Ada.Tags.Tag;

      C : Component_Lists.Cursor := E.Components.First;
      Comp : Component'Class := Null_Component;
   begin
      while Component_Lists.Has_Element (C) loop
         if CT = Component_Lists.Element (C)'Tag then
            Comp := Component_Lists.Element (C);
            exit;
         end if;

         Component_Lists.Next (C);
      end loop;

      return Comp;
   end Get_Component;

   ------------
   -- Render --
   ------------

   procedure Render (E : in Entity) is
      Comp : Component'Class := Get_Component (E, Display'Tag);
   begin
      if Comp in Display'Class then
         declare
            D : Display := Display (Comp);
         begin
            D.Render (E);
         end;
      end if;
   end Render;

   ----------
   -- Move --
   ----------

   procedure Move (E : in out Entity; dT : in Float) is
      P : constant Component'Class := Get_Component (E, Position_2D'Tag);
   begin
      if P'Tag in Position_2D'Tag then
         declare
            P_2D : Position_2D := Position_2D (P);
         begin
            P_2D.Position.X := P_2D.Position.X +
              0.5 * P_2D.Acceleration.X * dT ** 2 + P_2D.Velocity.X * dT;
            P_2D.Position.Y := P_2D.Position.Y +
              0.5 * P_2D.Acceleration.Y * dT ** 2 + P_2D.Velocity.Y * dT;

            P_2D.Velocity.X := P_2D.Velocity.X +
              P_2D.Acceleration.X * dT;
            P_2D.Velocity.Y := P_2D.Velocity.Y +
              P_2D.Acceleration.Y * dT;
         end;
      end if;
   end Move;

   -----------------
   -- Load_Bitmap --
   -----------------

   procedure Load_Bitmap (DB : Display_Bitmap; Filename : String) is
   begin
      DB.Bitmap := Bitmap_IO.al_load_bitmap (Interfaces.C.Strings.New_String (Filename));
   end Load_Bitmap;

   ------------
   -- Render --
   ------------

   procedure Render (DB : in Display_Bitmap; E : in Entity) is
      P : constant Component'Class := Get_Component (E, Position_2D'Tag);
   begin
      if P'Tag in Position_2D'Tag then
         declare
            P_2D : constant Position_2D := Position_2D (P);
         begin
            Bitmap_Draw.al_draw_bitmap (bitmap => DB.Bitmap,
                                        dx     => P_2D.Position.X,
                                        dy     => P_2D.Position.Y,
                                        flags  => 0);
         end;
      end if;
   end Render;

end Stardust_Engine.Entities;
