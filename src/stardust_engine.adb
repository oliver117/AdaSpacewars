with Ada.Calendar;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;

with Interfaces.C; use Interfaces.C;


with Allegro5.Color;
with Allegro5.Drawing;
with Allegro5.Events;
with Allegro5.Keyboard;
with Allegro5.Keycodes;
with Allegro5.System;

with Allegro5.Allegro.Primitives;

use  Allegro5; use Allegro5.Allegro;

package body Stardust_Engine is

   procedure Draw is
      procedure Each (Cur : Object_Lists.Cursor) is
         Obj : Object'Class := Object_Lists.Element (Cur);
      begin
         if Obj in Drawable'Class then
            Drawable'Class (Obj).Draw;
         end if;
      end Each;
   begin
      Drawing.al_clear_to_color (Color.al_map_rgb (r => 0,
                                                   g => 0,
                                                   b => 0));

      Object_List.Iterate (Each'Access);

      Display.al_flip_display;
   end Draw;

   procedure Move (dT : Duration) is
      procedure Each (Cur : Object_Lists.Cursor) is
         Obj : Object'Class := Object_Lists.Element (Cur);
      begin
         if Obj in Movable'Class then
            Movable'Class (Obj).Move(dT);
         end if;
      end Each;
   begin
      Object_List.Iterate (Each'Access);
   end Move;

   overriding
   procedure Move (Obj : in out Object_2; dT : Duration) is
   begin
      Obj.Pos.X := Obj.Pos.X + Obj.Vel.Vx * Float (dT);
      Obj.Pos.Y := Obj.Pos.Y + Obj.Vel.Vy * Float (dT);
   end Move;

   function Initialize (Width : Integer;
                        Height : Integer) return Boolean is
      Success : Boolean := True;
   begin
      Ada.Text_IO.Put ("System ... ");
      if System.al_init = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         goto deinit_system;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Ada.Text_IO.Put ("Keyboard ... ");
      if Keyboard.al_install_keyboard = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         Success := False;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Ada.Text_IO.Put ("Primitives ... ");
      if Allegro.Primitives.al_init_primitives_addon = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         Success := False;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Event_Queue := Events.al_create_event_queue;

      Screen_Width := int (Width);
      Screen_Height := int (Height);

      Disp := Display.al_create_display (w => Screen_Width,
                                         h => Screen_Height);

      Move_Timer := Timer.al_create_timer (1.0 / 60.0); -- 60 fps

      Events.al_register_event_source (Event_Queue, Keyboard.al_get_keyboard_event_source);
      Events.al_register_event_source (Event_Queue, Timer.al_get_timer_event_source (Move_Timer));
      Events.al_register_event_source (Event_Queue, Display.al_get_display_event_source (Disp));

      return Success;

      <<deinit_system>>
      System.al_uninstall_system;
      return False;
   end Initialize;

   procedure Cleanup is
   begin
      Display.al_destroy_display (Disp);
      Timer.al_destroy_timer (Move_Timer);
      Events.al_destroy_event_queue (Event_Queue);
      System.al_uninstall_system;
   end Cleanup;

   procedure Star_Timer is
   begin
      Timer.al_start_timer (Move_Timer);
   end Star_Timer;

   procedure Event_Loop is
      Event : aliased Events.ALLEGRO_EVENT;
   begin
      Events.al_wait_for_event (Event_Queue, Event'Access);

      if Event.c_type = Events.ALLEGRO_EVENT_DISPLAY_CLOSE then
         Close := True;
      elsif Event.c_type = Events.ALLEGRO_EVENT_KEY_DOWN and then
        Event.keyboard.keycode = Keycodes.ALLEGRO_KEY_ESCAPE then
         Close := True;
      elsif Event.c_type = Events.ALLEGRO_EVENT_TIMER then
         Move (1.0 / 60.0);
         Redraw := True;
      end if;

      if Events.al_is_event_queue_empty (Event_Queue) /= 0 and Redraw then
         Draw;
         Redraw := False;
      end if;
   end Event_Loop;






   function Absolute_Velocity (Obj : Object_X) return Float is
   begin
      return Elementary_Functions.Sqrt (Obj.Vx ** 2 + Obj.Vy ** 2);
   end Absolute_Velocity;

   -- dT : elapsed time
   procedure Move (Obj : in out Object_X; dT : Duration) is
   begin
      Obj.X := Obj.X + Obj.Vx * Float (dT);
      Obj.Y := Obj.Y + Obj.Vy * Float (dT);
   end Move;

   procedure Draw (Sp : Spaceship) is
   begin
      case Sp.Player is
         when One =>
            Primitives.al_draw_circle
              (cx        => Sp.X,
               cy        => Sp.Y,
               r         => 10.0,
               color     => Color.al_map_rgb (r => 100, g => 100, b => 255),
               thickness => 2.0);
            Primitives.al_draw_line
              (x1        => Sp.X,
               y1        => Sp.Y,
               x2        => Sp.X +
                            Elementary_Functions.Cos (Sp.Rotation) * 10.0,
               y2        => Sp.Y +
                            Elementary_Functions.Sin (Sp.Rotation) * 10.0,
               color     => Color.al_map_rgb (r => 255, g => 100, b => 100),
               thickness => 2.0);
         when Two =>
            Primitives.al_draw_circle
              (cx        => Sp.X,
               cy        => Sp.Y,
               r         => 10.0,
               color     => Color.al_map_rgb (r => 100, g => 100, b => 255),
               thickness => 2.0);
            Primitives.al_draw_line
              (x1        => Sp.X,
               y1        => Sp.Y,
               x2        => Sp.X +
                            Elementary_Functions.Cos (Sp.Rotation) * 10.0,
               y2        => Sp.Y +
                            Elementary_Functions.Sin (Sp.Rotation) * 10.0,
               color     => Color.al_map_rgb (r => 100, g => 255, b => 100),
               thickness => 2.0);
      end case;
   end Draw;

   procedure Draw (S : Star) is
      Com : unsigned_char       := unsigned_char (255.0 * S.Luminance);
      Col : Color.ALLEGRO_COLOR :=
         Color.al_map_rgb (r => Com, g => Com, b => Com);
   begin
      Drawing.al_draw_pixel (x => S.X, y => S.Y, color => Col);
   end Draw;

   procedure Create_Starmap is
   begin
      for I in Starmap'Range loop
         Starmap (I) :=
           Star'
           (X         => Float (Screen_Width) *
                         Float_Random.Random (Float_RNG),
            Y         => Float (Screen_Height) *
                         Float_Random.Random (Float_RNG),
            Vx        => 0.0,
            Vy        => 0.0,
            Rotation  => 0.0,
            Luminance => Float_Random.Random (Float_RNG));
      end loop;
   end Create_Starmap;

   procedure Draw_Starmap is
   begin
      for I in Starmap'Range loop
         Starmap (I).Draw;
      end loop;
   end Draw_Starmap;

   procedure Draw_Players is
   begin
      for Pn in Player_Number'Range loop
         Players (Pn).Draw;
      end loop;
   end Draw_Players;

   procedure Move (PI_1 : Player_Input; PI_2 : Player_Input; dT : Float) is
      function Wrap (Val : Float; Limit : Float) return Float is
      begin
         if Val > Limit then
            return 0.0;
         elsif Val < 0.0 then
            return Limit;
         else
            return Val;
         end if;
      end Wrap;
   begin
      declare
         P : Spaceship := Players (One);
      begin
         if PI_1.Turn_Left then
            P.Rotation := P.Rotation - Float (Pi) * dT;
         end if;
         if PI_1.Turn_Right then
            P.Rotation := P.Rotation + Float (Pi) * dT;
         end if;
         if PI_1.Accelerate then
            P.Vx := P.Vx +
                    100.0 * dT * Elementary_Functions.Cos (P.Rotation);
            P.Vy := P.Vy +
                    100.0 * dT * Elementary_Functions.Sin (P.Rotation);
         end if;
         if PI_1.Decelerate then
            P.Vx := P.Vx -
                    100.0 * dT * Elementary_Functions.Cos (P.Rotation);
            P.Vy := P.Vy -
                    100.0 * dT * Elementary_Functions.Sin (P.Rotation);
         end if;
         if PI_1.Fire then
            Projectile_Register.Append
              (New_Item =>
                 Projectile'(X        => P.X,
                             Y        => P.Y,
                             Vx       =>
              Elementary_Functions.Cos ((P.Rotation)) * 1_000.0,
                             Vy       =>
              Elementary_Functions.Sin ((P.Rotation)) * 1_000.0,
                             C        => Color.al_map_rgb
                                           (r => 255,
                                            g => 100,
                                            b => 100),
                             Rotation => Players (One).Rotation,
                             Shooter  => One));
         end if;

         P.X := P.X + P.Vx * dT;
         P.Y := P.Y + P.Vy * dT;

         P.X := Wrap (P.X, Float (Screen_Width));
         P.Y := Wrap (P.Y, Float (Screen_Height));

         Players (One) := P;
      end;

      declare
         P : Spaceship := Players (Two);
      begin
         if PI_2.Turn_Left then
            P.Rotation := P.Rotation - Float (Pi) * dT;
         end if;
         if PI_2.Turn_Right then
            P.Rotation := P.Rotation + Float (Pi) * dT;
         end if;
         if PI_2.Accelerate then
            P.Vx := P.Vx +
                    100.0 * dT * Elementary_Functions.Cos (P.Rotation);
            P.Vy := P.Vy +
                    100.0 * dT * Elementary_Functions.Sin (P.Rotation);
         end if;
         if PI_2.Decelerate then
            P.Vx := P.Vx -
                    100.0 * dT * Elementary_Functions.Cos (P.Rotation);
            P.Vy := P.Vy -
                    100.0 * dT * Elementary_Functions.Sin (P.Rotation);
         end if;
         if PI_2.Fire then
            Projectile_Register.Append
              (New_Item =>
                 Projectile'(X        => P.X,
                             Y        => P.Y,
                             Vx       =>
              Elementary_Functions.Cos ((P.Rotation)) * 1_000.0,
                             Vy       =>
              Elementary_Functions.Sin ((P.Rotation)) * 1_000.0,
                             C        => Color.al_map_rgb
                                           (r => 100,
                                            g => 255,
                                            b => 100),
                             Rotation => Players (Two).Rotation,
                             Shooter  => Two));
         end if;

         P.X := P.X + P.Vx * dT;
         P.Y := P.Y + P.Vy * dT;

         P.X := Wrap (P.X, Float (Screen_Width));
         P.Y := Wrap (P.Y, Float (Screen_Height));

         Players (Two) := P;
      end;

      declare
         Cur : Projectile_Doubly_Linked_Lists.Cursor :=
            Projectile_Register.First;
         use Projectile_Doubly_Linked_Lists;
      begin
         while Has_Element (Cur) loop
            -- bound check
            if Element (Cur).X < 0.0 or
               Element (Cur).X > Float (Screen_Width) or
               Element (Cur).Y < 0.0 or
               Element (Cur).Y > Float (Screen_Height)
            then
               Projectile_Register.Delete (Cur);
            else
               -- move
               declare
                  Proj : Projectile := Element (Cur);
               begin
                  Proj.Move (dT => 0.01);
                  Projectile_Register.Replace_Element
                    (Position => Cur,
                     New_Item => Proj);
               end;

               -- collision detect
               if Element (Cur).Shooter = One then
                  declare
                     Distance : Float;
                  begin
                     Distance := (Element (Cur).X - Players (Two).X) ** 2 +
                                 (Element (Cur).Y - Players (Two).Y) ** 2;
                     if Distance < 100.0 then
                        Players (One).Hits := Players (One).Hits + 1;
                        Projectile_Register.Delete (Cur);
                        Ada.Text_IO.Put_Line ("Player one hit!, Hits: " & Integer'Image (Players (One).Hits));
                     end if;
                  end;
               else
                  declare
                     Distance : Float;
                  begin
                     Distance := (Element (Cur).X - Players (One).X) ** 2 +
                                 (Element (Cur).Y - Players (One).Y) ** 2;
                     if Distance < 100.0 then
                        Players (Two).Hits := Players (Two).Hits + 1;
                        Projectile_Register.Delete (Cur);
                        Ada.Text_IO.Put_Line ("Player two hit!, Hits: " & Integer'Image (Players (Two).Hits));
                     end if;
                  end;
               end if;

            end if;

            Next (Cur);
         end loop;
      end;
   end Move;

   procedure Draw (P : Projectile) is
      Col : Color.ALLEGRO_COLOR;
   begin
      for I in Integer range 1 .. 10  loop
         Col := Color.ALLEGRO_COLOR'(r => P.C.r,
                                     g => P.C.g,
                                     b => P.C.b,
                                     a => Float (I) * 0.1);

         Primitives.al_draw_filled_circle (cx    => P.X - 2.0 * Float (I) * Elementary_Functions.Cos (P.Rotation),
                                           cy    => P.Y - 2.0 * Float (I) * Elementary_Functions.Sin (P.Rotation),
                                           r     => 2.0,
                                           color => Col);
      end loop;
   end Draw;

   procedure Render_Projectiles is
   begin

      declare
         Cur : Projectile_Doubly_Linked_Lists.Cursor :=
            Projectile_Register.First;
         use Projectile_Doubly_Linked_Lists;
      begin
         while Has_Element (Cur) loop
            --render
            Draw (Element (Cur));

            Next (Cur);
         end loop;
      end;
   end Render_Projectiles;

   function Get_Screen_Width return int is
   begin
      return Screen_Width;
   end Get_Screen_Width;

   function Get_Screen_Height return int is
   begin
      return Screen_Height;
   end Get_Screen_Height;

   function Want_Close return Boolean is
   begin
      return Close;
   end Want_Close;

begin
   Players (One).X        := 100.0;
   Players (One).Y        := 100.0;
   Players (One).Rotation := Pi * 1.0 / 4.0;
   Players (One).Player   := One;

   Players (Two).X        := Float (Screen_Width - 100);
   Players (Two).Y        := Float (Screen_Height - 100);
   Players (Two).Rotation := Pi * 5.0 / 4.0;
   Players (Two).Player   := Two;

end Stardust_Engine;
