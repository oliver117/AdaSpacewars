with Ada.Text_IO;

with Allegro5.Bitmap_Draw;
with Allegro5.Bitmap_IO;
with Allegro5.Drawing;

with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;

with Interfaces.C.Strings;

with Stardust_Engine.Entities.Components;



package body Stardust_Engine.Entities is

   package Get_Position_2D is new Components (Component_Type => Position_2D);
   package Get_Rotation is new Components (Component_Type => Rotation);


   ----------------
   -- New_Entity --
   ----------------

   function New_Entity return Entity is
   begin
      return Entity'(Components => Component_Lists.Empty_List);
   end New_Entity;

   -------------------
   -- Reset_Entity --
   -------------------

   procedure Reset_Entity (E : in out Entity) is
   begin
      E := New_Entity;
   end Reset_Entity;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component (E : in out Entity; C : in Component'Class) is
   begin
      E.Components.Append (C);
   end Add_Component;

   --------------------
   -- Has_Components --
   --------------------

   function Has_Components (E : Entity) return Boolean is
   begin
      return not E.Components.Is_Empty;
   end Has_Components;

   ----------------------
   -- Count_Components --
   ----------------------

   function Count_Components (E : Entity) return Natural is
   begin
      return Natural (E.Components.Length);
   end Count_Components;

   ------------
   -- Render --
   ------------

   procedure Render (E : in Entity) is
   begin
      null;
   end Render;

   ----------
   -- Move --
   ----------

   procedure Move (E : in out Entity; dT : in Float) is
      Position_Cursor : constant Get_Position_2D.Cursor := Get_Position_2D.First (E);
      Attitude_Cursor : constant Get_Rotation.Cursor := Get_Rotation.First (E);
   begin
      if Get_Position_2D.Has_Component (Position_Cursor) then
         declare
            P_2D : Position_2D := Get_Position_2D.Component (Position_Cursor);
         begin
            P_2D.Position.X := P_2D.Position.X +
              0.5 * P_2D.Acceleration.X * dT ** 2 +
                P_2D.Velocity.X * dT;
            P_2D.Position.Y := P_2D.Position.Y +
              0.5 * P_2D.Acceleration.Y * dT ** 2 +
                P_2D.Velocity.Y * dT;

            if Wrap_Around then
               if P_2D.Position.X < 0.0 then
                  P_2D.Position.X := P_2D.Position.X + Float (Get_Screen_Width);
               elsif P_2D.Position.X > Float (Get_Screen_Width) then
                  P_2D.Position.X := P_2D.Position.X - Float (Get_Screen_Width);
               end if;

               if P_2D.Position.Y < 0.0 then
                  P_2D.Position.Y := P_2D.Position.Y + Float (Get_Screen_Height);
               elsif P_2D.Position.Y > Float (Get_Screen_Height) then
                  P_2D.Position.Y := P_2D.Position.Y - Float (Get_Screen_Height);
               end if;
            end if;

            P_2D.Velocity.X := P_2D.Velocity.X +
              P_2D.Acceleration.X * dT;
            P_2D.Velocity.Y := P_2D.Velocity.Y +
              P_2D.Acceleration.Y * dT;

            Get_Position_2D.Replace_Component (E, Position_Cursor, P_2D);
         end;
      end if;

      if Get_Rotation.Has_Component (Attitude_Cursor) then
         declare
            Rot : Rotation := Get_Rotation.Component (Attitude_Cursor);
         begin
            Rot.Angle := Rot.Angle +
              0.5 * Rot.Angular_Acceleration * dT ** 2 +
                Rot.Angular_Velocity * dT;

            Rot.Angular_Velocity := Rot.Angular_Velocity +
              Rot.Angular_Acceleration * dT;

            Get_Rotation.Replace_Component (E, Attitude_Cursor, Rot);
         end;
      end if;
   end Move;

   -----------------
   -- Load_Bitmap --
   -----------------

   procedure Load_Bitmap (B : in out Bitmap'Class; Filename : in String) is
   begin
      B.Bitmap := Bitmap_IO.al_load_bitmap (Interfaces.C.Strings.New_String (Filename));
   end Load_Bitmap;

   ------------
   -- Render --
   ------------

   procedure Render (P : in Pixel; E : in Entity) is
      Position_Cursor : constant Get_Position_2D.Cursor := Get_Position_2D.First (E);
   begin
      if Get_Position_2D.Has_Component (Position_Cursor) then
         declare
            P_2D : constant Position_2D := Get_Position_2D.Component (Position_Cursor);
         begin
            Drawing.al_draw_pixel (x     => P_2D.Position.X,
                                   y     => P_2D.Position.Y,
                                   color => P.Color);
         end;
      end if;
   end Render;


   procedure Render (B : in Bitmap; E : in Entity) is
   begin
      null;
   end Render;

   procedure Render (B : in Tinted_Scaled_Rotated_Bitmap; E : in Entity) is
--        P : constant Component'Class := Get_Component (E, Position_2D'Tag);
--        A : constant Component'Class := Get_Component (E, Rotation'Tag);
--     begin
--        if P'Tag in Position_2D'Tag and A'Tag in Rotation'Tag then
--           declare
--              P_2D : constant Position_2D := Position_2D (P);
--              Rot : constant Rotation := Rotation (A);
--
--              cX : constant Float := Float (Allegro5.Bitmap.al_get_bitmap_width (B.Bitmap)) / 2.0;
--              cY : constant Float := Float (Allegro5.Bitmap.al_get_bitmap_height (B.Bitmap)) / 2.0;
--              dX : constant Float := P_2D.Position.X - cX * B.Scale.X;
--              dY : constant Float := P_2D.Position.Y - cY * B.Scale.Y;
--           begin
--              Ada.Text_IO.Put_Line ("dx: " & Float'Image (dx));
--              Drawing.al_draw_pixel (x     => dX,
--                                     y     => dY,
--                                     color => Color.al_map_rgb (255, 255, 255));
--              Bitmap_Draw.al_draw_tinted_scaled_rotated_bitmap (bitmap => B.Bitmap,
--                                                                tint   => B.Tint,
--                                                                cx     => cX,
--                                                                cy     => cY,
--                                                                dx     => dX,
--                                                                dy     => dY,
--                                                                xscale => B.Scale.X,
--                                                                yscale => B.Scale.Y,
--                                                                angle  => Rot.Angle,
--                                                                flags  => 0);
--           end;
--        end if;
   begin
      null;
   end render;

   -------------
   -- Process --
   -------------

   overriding
   procedure Process (C : in Acceleration_Control; E : in out Entity) is
--        use Ada.Numerics;
--        use Ada.Numerics.Elementary_Functions;
--
--        P : constant Component_Lists.Reference_Type := Get_Component_Reference (E, Position_2D'Tag);
--        A : constant Component'Class := Get_Component (E, Rotation'Tag);
   begin
--        if P.Element'Tag in Position_2D'Tag and then A'Tag in Rotation'Tag then
--           declare
--              P_2D : Position_2D := Position_2D (P.Element.all);
--              Rot : constant Rotation := Rotation (A);
--           begin
--              if Key_Pressed (C.Accelerate) and Key_Pressed (C.Decelerate) then
--                 null; -- both keys pressed, do nothing
--              elsif Key_Pressed (C.Accelerate) then
--                 P_2D.Acceleration.X := C.Rate * Cos (Rot.Angle - Pi / 2.0);
--                 P_2D.Acceleration.Y := C.Rate * Sin (Rot.Angle - Pi / 2.0);
--              elsif Key_Pressed (C.Decelerate) then
--                 P_2D.Acceleration.X := -C.Rate * Cos (Rot.Angle - Pi / 2.0);
--                 P_2D.Acceleration.Y := -C.Rate * Sin (Rot.Angle - Pi / 2.0);
--              else
--                 -- no key press, set acceleration to zero
--                 P_2D.Acceleration.X := 0.0;
--                 P_2D.Acceleration.Y := 0.0;
--              end if;
--
--              P.Element.all := Component'Class (P_2D);
--           end;
--        end if;
null;
   end Process;

   overriding
   procedure Process (C : in Turn_Control; E : in out Entity) is
--        A : constant Component_Lists.Reference_Type := Get_Component_Reference (E, Rotation'Tag);
--     begin
--       if A.Element'Tag in Rotation'Tag then
--           declare
--              Rot : Rotation := Rotation (A.Element.all);
--           begin
--              if Key_Pressed (C.Turn_Left) and Key_Pressed (C.Turn_Right) then
--                 null; -- both keys pressed
--              elsif Key_Pressed (C.Turn_Left) then
--                 Rot.Angular_Velocity := -C.Rate;
--              elsif Key_Pressed (C.Turn_Right) then
--                 Rot.Angular_Velocity := C.Rate;
--              else
--                 Rot.Angular_Velocity := 0.0;
--              end if;
--
--              A.Element.all := Component'Class (Rot);
--           end;
--        end if;
begin null;
   end Process;

end Stardust_Engine.Entities;
