with Ada.Numerics;
with GNAT.Random_Numbers;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

with Allegro5.Base;
with Allegro5.Color;
with Allegro5.Display;
with Allegro5.Drawing;
with Allegro5.Error;
with Allegro5.Events;
with Allegro5.Keyboard;
with Allegro5.Keycodes;
with Allegro5.Allegro.Primitives;
with Allegro5.System;
with Allegro5.Timer;
with Allegro5.Transformations;
use Allegro5;
use Allegro5.Allegro;

with Stardust_Engine; use Stardust_Engine;
with Stardust_Engine.Particles;

procedure Spacewars is
   procedure Draw (P : Particles.Particle) is
   begin
      if P.TTL > 0 then
         Drawing.al_draw_pixel (x     => P.Pos.X,
                                y     => P.Pos.Y,
                                color => P.Color);
      end if;
   end Draw;

   procedure Move (P : in out Particles.Particle; dT : Duration) is
   begin
      if P.TTL <= 0 then
         P.TTL := 300;
         P.Pos := Position_2'(400.0, 300.0);
      end if;
      Object_2 (P).Move (dT);
   end Move;

   package Particle_Sprayer is new Particles.Particle_System (Particle_T    => Particles.Particle,
                                                              Draw_Particle => Draw,
                                                              Move_Particle => Move);

   type Spaceship is new Object_2 and Drawable with null record;

   Player_1 : Spaceship := Spaceship'(Pos => Position_2'(X => 100.0,
                                                         Y => 100.0),
                                      Vel => Velocity_2'(Vx => 0.0,
                                                         Vy => 0.0));

   procedure Draw (Sp : Spaceship) is
   begin

      -- left
      Primitives.al_draw_line (x1        => Sp.Pos.X,
                               y1        => Sp.Pos.Y - 10.0,
                               x2        => Sp.Pos.X - 10.0,
                               y2        => Sp.Pos.Y + 7.0,
                               color     => Color.al_map_rgb_f (0.5, 1.0, 0.5),
                               thickness => 1.0);
      -- right
      Primitives.al_draw_line (x1        => Sp.Pos.X,
                               y1        => Sp.Pos.Y - 10.0,
                               x2        => Sp.Pos.X + 10.0,
                               y2        => Sp.Pos.Y + 7.0,
                               color     => Color.al_map_rgb_f (0.5, 1.0, 0.5),
                               thickness => 1.0);
      -- bottom
      Primitives.al_draw_line (x1        => Sp.Pos.X - 10.0,
                               y1        => Sp.Pos.Y + 7.0,
                               x2        => Sp.Pos.X + 10.0,
                               y2        => Sp.Pos.Y + 7.0,
                               color     => Color.al_map_rgb_f (0.5, 1.0, 0.5),
                               thickness => 1.0);
   end Draw;

   Trans : aliased Transformations.ALLEGRO_TRANSFORM;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if not Initialize (800, 600) then
      return;
   end if;

   Transformations.al_identity_transform (Trans'Access);
   Transformations.al_translate_transform (Trans'Access, 10.0, 10.0);
   Transformations.al_scale_transform (Trans'Access, 2.0, 3.0);
   Transformations.al_rotate_transform (Trans'Access, 0.1);
   Transformations.al_use_transform (Trans);

   declare
      Angle : Float;
      Speed : Float;

      use Ada.Numerics.Elementary_Functions;
      use GNAT.Random_Numbers;
      RNG : Generator;
   begin
      for I in Integer range 1 .. 1_000 loop
         Angle := Random_Gaussian (RNG) * 1.0 * Ada.Numerics.Pi;
         Speed := Sqrt (Random (RNG) * 350.0);

         Particle_Sprayer.Particles.Append (Particles.Particle'(Pos   => Position_2'(400.0, 300.0),
                                                                Vel   => Velocity_2'(Speed * Cos (Angle), Speed * Sin (Angle)),
                                                                TTL   => Integer (Float'(Random (RNG)) * 100.0) + 100,
                                                                Color => Color.al_map_rgb_f (r => Random (RNG),
                                                                                             g => Random (RNG),
                                                                                             b => Random (RNG))));
      end loop;
   end;


   Object_List.Append (Particle_Sprayer.Get_Handle);
   Object_List.Append (Player_1);

   Star_Timer;

   Main_Loop:
   loop
      Event_Loop;

      if Want_Close then
         goto cleanup;
      end if;

   end loop Main_Loop;

   <<cleanup>>
   Stardust_Engine.Cleanup;

end Spacewars;
