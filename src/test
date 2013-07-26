with Ada.Text_IO;
with Interfaces.c;
with interfaces.c.Extensions;
with Interfaces.c.Strings;

use interfaces.c;

with Allegro5.Altime;
with Allegro5.Base;
with Allegro5.Display;
with Allegro5.Drawing;
with Allegro5.Color;
with Allegro5.System;
with Allegro5.Allegro.Primitives;
use Allegro5.Allegro;
with Allegro5.Joystick;

use Allegro5;


with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;
use Ada.Numerics;

with Ada.Containers.Doubly_Linked_Lists;

procedure Test is
   D: Display.ALLEGRO_DISPLAY;
   red: Color.ALLEGRO_COLOR;

   use Interfaces.C;

   sticks : int;
   stick : Joystick.ALLEGRO_JOYSTICK;
   joy_state : Joystick.ALLEGRO_JOYSTICK_STATE;

   type Object is tagged record
      X: float; -- position
      Y: float;
      Vx: float; -- velocity
      Vy: float;
   end record;

   procedure Move (Obj : in out Object'Class; dT : float) is
   begin
      Obj.X := Obj.X + Obj.Vx * dT;
      Obj.Y := Obj.Y + Obj.Vy * dT;
   end Move;

   type Projectile is new Object with record
      C: Color.ALLEGRO_COLOR := Color.al_map_rgb (r => 200,
                                                  g => 200,
                                                  b => 255);
   end record;

   package Projectile_Doubly_Linked_Lists is new Ada.Containers.Doubly_Linked_Lists (Projectile);

   Projectile_Register : Projectile_Doubly_Linked_Lists.List;

   type Spaceship is new Object with record
      Rotation: float; -- angle
   end record;

   Screen_Height : constant := 600;
   Screen_Width : constant := 800;

   Player : Spaceship := Spaceship'(X        => float (Screen_Width) / 2.0,
                                    Y        => float (Screen_Height) / 2.0,
                                    Vx       => 0.0,
                                    Vy       => 0.0,
                                    Rotation => 0.0);

   Player_dV : float := 0.0;

   type Star is new Object with record
      Luminance: float;
   end record;

   type Star_Array is array (Integer range <>) of Star;

   Starmap : Star_Array (0 .. 2_000);

   float_RNG : Float_Random.Generator;

   function Deg_To_Rad (Deg : float) return float is
   begin
      return Ada.Numerics.Pi * Deg / 180.0;
   end Deg_To_Rad;

   Fired : Boolean := False;

begin
   if System.al_init > 1 then
      Ada.Text_IO.Put_Line ("failed init");
   end if;

   if Joystick.al_install_joystick > 1 then
      Ada.Text_IO.Put_Line ("failed init");
   end if;


   -- init stars
   for I in Starmap'Range loop
      Starmap (I).X := float (Screen_Width) * Float_Random.Random (float_RNG);
      Starmap (I).Y := float (Screen_Height) * Float_Random.Random (float_RNG);
      Starmap (I).Luminance := Float_Random.Random (float_RNG);
   end loop;

   stick := Joystick.al_get_joystick (joyn =>  0);

   Display.al_set_new_display_option(Display.ALLEGRO_SAMPLE_BUFFERS, 1, 1);
   Display.al_set_new_display_option(Display.ALLEGRO_SAMPLES, 8, 1);

   D := Display.al_create_display (Screen_Width, Screen_Height);

   red := Color.al_map_rgb(255, 0, 0);

   if Primitives.al_init_primitives_addon > 1 then
      Ada.Text_IO.Put_Line ("failed init");
   end if;

   loop
      Joystick.al_get_joystick_state (stick, joy_state);

      -- clear surface
      Drawing.al_clear_to_color (Color.al_map_rgb(0, 0, 0));

      -- draw stars
      for I in Starmap'Range loop
         Drawing.al_draw_pixel (Starmap (I).X, Starmap (I).Y, Color.al_map_rgb_f (Starmap (I).Luminance, Starmap (I).Luminance, Starmap (I).Luminance));
      end loop;

      -- draw spaceship
      Primitives.al_draw_circle (cx        => Player.X,
                                 cy        => Player.Y,
                                 r         => 10.0,
                                 color     => Color.al_map_rgb (r => 100,
                                                                g => 100,
                                                                b => 255),
                                 thickness => 2.0);

      -- cannon
      Primitives.al_draw_line (x1        => Player.X,
                               y1        => Player.Y,
                               x2        => Player.X + Elementary_Functions.Cos (Deg_To_Rad (Player.Rotation)) * 10.0,
                               y2        => Player.Y + Elementary_Functions.Sin (Deg_To_Rad (Player.Rotation)) * 10.0,
                               color     => color.al_map_rgb (r => 255,
                                                              g => 100,
                                                              b => 100),
                               thickness => 2.0);


      -- respond to joy button 1 by drawing the ellipse filled if pressed
      if  joy_state.button (0) > 1 then
         if Fired = False then
            Projectile_Register.Append (New_Item => Projectile'(X  => Player.X,
                                                                Y  => Player.Y,
                                                                Vx => Elementary_Functions.Cos (Deg_To_Rad (Player.Rotation)) * 1_000.0,
                                                                Vy => Elementary_Functions.Sin (Deg_To_Rad (Player.Rotation)) * 1_000.0,
                                                                C => <>));
         end if;

         Fired := True;
      else

         Fired := False;
      end if;

      if joy_state.button (1) > 1 then
         return;
      end if;


      -- render projectiles

      declare
         Cur : Projectile_Doubly_Linked_Lists.Cursor := Projectile_Register.First;
         use Projectile_Doubly_Linked_Lists;
      begin
         while Has_Element (Cur) loop
            -- bound check
            if Element (Cur).X < 0.0 or Element (Cur).X > float (Screen_Width) or
               Element (Cur).Y < 0.0 or Element (Cur).Y > float (Screen_Height) then
               Projectile_Register.Delete (Cur);
            else
               -- move
               declare
                  Proj : Projectile := Element (Cur);
               begin
                  Proj.Move (dT  => 0.01);
                  Projectile_Register.Replace_Element (Position => Cur,
                                                       New_Item => Proj);
               end;
               --render
               Primitives.al_draw_filled_circle (cx        => Element (Cur).X,
                                                 cy        => Element (Cur).Y,
                                                 r         => 2.0,
                                                 color     => Element (Cur).C);
            end if;

            Next (Cur);
         end loop;
      end;


      Allegro5.Display.al_flip_display;


      Player.Rotation := Player.Rotation + joy_state.stick (0).axis (0) * 10.0;
      Player_dV := joy_state.stick (0).axis (1) * (-10.0);

      Player.Vx := Player.Vx + Elementary_Functions.Cos (Deg_To_Rad (Player.Rotation)) * Player_dV;
      Player.Vy := Player.Vy + Elementary_Functions.Sin (Deg_To_Rad (Player.Rotation)) * Player_dV;

      -- move player
      Player.Move (dT => 0.01);

      -- if the player exits the stage, he will be transported to the other side
      if Player.X > float (Screen_Width) then
         Player.X := 0.0;
      elsif Player.X < 0.0 then
         Player.X := float (Screen_Width);
      end if;

      if Player.Y > float (Screen_Height) then
         Player.Y := 0.0;
      elsif Player.Y < 0.0 then
         Player.Y := float (Screen_Height);
      end if;


      Allegro5.Altime.al_rest (seconds => 0.01);
   end loop;

   Allegro5.Display.al_destroy_display (D);

   Ada.Text_IO.Put_Line ("done");
end Test;
