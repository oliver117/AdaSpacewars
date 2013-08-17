with Ada.Numerics;
with GNAT.Random_Numbers;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with Allegro5.Base;
with Allegro5.Bitmap;
with Allegro5.Bitmap_Draw;
with Allegro5.Bitmap_IO;
with Allegro5.Color;
with Allegro5.Display;
with Allegro5.Drawing;
with Allegro5.Error;
with Allegro5.Events;
with Allegro5.Addon.Font;
with Allegro5.Keyboard;
with Allegro5.Keycodes;
with Allegro5.Addon.Primitives;
with Allegro5.System;
with Allegro5.Timer;
with Allegro5.Transformations;
with Allegro5.Addon.TTF;
use Allegro5;
use Allegro5.Addon;

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
         P.TTL := 100;
         P.Pos := Position_2'(100.0, 120.0);
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

   Flyer1 : Bitmap.ALLEGRO_BITMAP;
   Flyer2 : Bitmap.ALLEGRO_BITMAP;
   Singularity : Bitmap.ALLEGRO_BITMAP;

   Rotation : Float := 0.0;

   procedure Draw (Sp : Spaceship) is
   begin
      Bitmap_Draw.al_draw_tinted_scaled_rotated_bitmap (bitmap => Flyer1,
                                                        tint   => Color.al_map_rgb_f(1.0, 0.2, 0.2),
                                                        cx     => 16.0,
                                                        cy     => 16.0,
                                                        dx     => 300.0,
                                                        dy     => 300.0,
                                                        xscale => 0.05,
                                                        yscale => 0.05,
                                                        angle  => Rotation,
                                                        flags  => 0);

      Bitmap_Draw.al_draw_tinted_scaled_rotated_bitmap (bitmap => Flyer2,
                                                        tint   => Color.al_map_rgb_f(0.2, 0.2, 1.0),
                                                        cx     => 16.0,
                                                        cy     => 16.0,
                                                        dx     => 100.0,
                                                        dy     => 100.0,
                                                        xscale => 0.05,
                                                        yscale => 0.05,
                                                        angle  => Rotation,
                                                        flags  => 0);

      Bitmap_Draw.al_draw_tinted_scaled_rotated_bitmap (bitmap => Singularity,
                                                        tint   => Color.al_map_rgb_f(1.0, 0.0, 1.0),
                                                        cx     => 16.0,
                                                        cy     => 16.0,
                                                        dx     => 500.0,
                                                        dy     => 500.0,
                                                        xscale => 1.0,
                                                        yscale => 1.0,
                                                        angle  => Rotation,
                                                        flags  => 0);
   end Draw;

   type Writing (Length : Natural) is new Drawable with
      record
         Pos : Position_2;
         Text : String (1 .. Length);
         Font : access Addon.Font.ALLEGRO_FONT;
         Color : Allegro5.Color.ALLEGRO_COLOR;
      end record;

   procedure Draw (W : Writing) is
   begin
      Font.al_draw_text (font  => W.Font,
                         color => W.Color,
                         x     => W.Pos.X,
                         y     => W.Pos.Y,
                         flags => Font.ALLEGRO_ALIGN_INTEGER,
                         text  => Interfaces.C.Strings.New_String (W.Text));
   end Draw;

   Title : Writing (12);
   Sax_Mono : access Font.ALLEGRO_FONT;

   Star_Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if not Initialize (1200, 800) then
      return;
   end if;

   Font.al_init_font_addon;
   if TTF.al_init_ttf_addon = 0 then
      Ada.Text_IO.Put_Line ("Failed to initialize TTF addon");
   end if;


   Flyer1 := Bitmap_IO.al_load_bitmap (Interfaces.C.Strings.New_String ("../flyer1.png"));
   Flyer2 := Bitmap_IO.al_load_bitmap (Interfaces.C.Strings.New_String ("../flyer2.png"));
   Singularity := Bitmap_IO.al_load_bitmap (Interfaces.C.Strings.New_String ("../singularity.png"));
   Sax_Mono := TTF.al_load_ttf_font (filename => Interfaces.C.Strings.New_String ("../font/saxmono.ttf"),
                                     size     => 64,
                                     flags    => 0);

   Title.Pos.X := 400.0;
   Title.Pos.Y := 400.0;
   Title.Text := String'("AdaSpacewars");
   Title.Font := Sax_Mono;
   Title.Color := Color.al_map_rgba_f (r => 1.0,
                                       g => 1.0,
                                       b => 1.0,
                                       a => 1.0);

   Star_Bitmap := Allegro5.Bitmap.al_create_bitmap (w => Get_Screen_Width,
                                                    h => Get_Screen_Height);

   declare
      Angle : Float;
      Speed : Float;

      use Ada.Numerics.Elementary_Functions;
      use GNAT.Random_Numbers;
      RNG : Generator;
   begin
      for I in Integer range 1 .. 100 loop
         Angle := Random_Gaussian (RNG) * 0.1 * Ada.Numerics.Pi + Ada.Numerics.PI / 2.0;
         Speed := Sqrt (Random (RNG) * 250.0) * 5.0;

         Particle_Sprayer.Particles.Append (Particles.Particle'(Pos   => Position_2'(100.0, 120.0),
                                                                Vel   => Velocity_2'(Speed * Cos (Angle), Speed * Sin (Angle)),
                                                                TTL   => Integer (Float'(Random (RNG)) * 100.0) + 50,
                                                                Color => Color.al_map_rgba_f (r => 0.0,
                                                                                              g => 1.0,
                                                                                              b => 0.0,
                                                                                              a => 0.5)));
      end loop;
   end;


   -- Object_List.Append (Particle_Sprayer.Get_Handle);
   Object_List.Append (Player_1);
   Object_List.Append (Title);

   Star_Timer;

   declare
      Event : Events.ALLEGRO_EVENT;
   begin
      Main_Loop:
      loop
         Event := Wait_For_Event;
         if Event.c_type = Events.ALLEGRO_EVENT_KEY_DOWN then
            if Event.keyboard.keycode = Keycodes.ALLEGRO_KEY_RIGHT then
               Rotation := Rotation + 0.01 * Ada.Numerics.Pi;
            end if;
         end if;

         Handle_Event (Event);

         if Want_Close then
            goto cleanup;
         end if;

      end loop Main_Loop;
   end;

   <<cleanup>>
   Stardust_Engine.Cleanup;

end Spacewars;
