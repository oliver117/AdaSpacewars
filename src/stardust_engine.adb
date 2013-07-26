with Ada.Calendar;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics;
with Ada.Numerics.Float_Random;

with Interfaces.C; use Interfaces.C;

with Allegro5.Bitmap;
with Allegro5.Color;
with Allegro5.Allegro.Primitives;

use Allegro5;
use Allegro5.Allegro;


package body Stardust_Engine is

   function Absolute_Velocity (Obj : Object) return Float is
   begin
      return Elementary_Functions.Sqrt (Obj.Vx ** 2 + Obj.Vy ** 2);
   end Absolute_Velocity;

   -- dT : elapsed time
   function Move (Obj : Object'Class; dT: Duration) return Object'Class is
      O : Object'Class := Obj;
   begin
      O.X := Obj.X + Obj.Vx * Float (dT);
      O.Y := Obj.Y + Obj.Vy * Float (dT);

      return O;
   end Move;

   procedure Move (Obj : in out Object'Class; dT: Duration) is
   begin
      Obj := Obj.Move (dT);
   end Move;

   procedure Draw (Sp : Spaceship) is
   begin
      case Sp.Player is
         when One =>
            Primitives.al_draw_circle (cx        => Sp.X,
                                       cy        => Sp.Y,
                                       r         => 10.0,
                                       color     => Color.al_map_rgb (r => 100,
                                                                      g => 100,
                                                                      b => 255),
                                       thickness => 2.0);
            Primitives.al_draw_line (x1        => Sp.X,
                                     y1        => Sp.Y,
                                     x2        => Sp.X + Elementary_Functions.Cos (Sp.Rotation) * 10.0,
                                     y2        => Sp.Y + Elementary_Functions.Sin (Sp.Rotation) * 10.0,
                                     color     => Color.al_map_rgb (r => 255,
                                                                    g => 100,
                                                                    b => 100),
                                     thickness => 2.0);
         when Two =>
            Primitives.al_draw_circle (cx        => Sp.X,
                                       cy        => Sp.Y,
                                       r         => 10.0,
                                       color     => Color.al_map_rgb (r => 100,
                                                                      g => 255,
                                                                      b => 100),
                                       thickness => 2.0);
            Primitives.al_draw_line (x1        => Sp.X,
                                     y1        => Sp.Y,
                                     x2        => Sp.X + Elementary_Functions.Cos (Sp.Rotation) * 10.0,
                                     y2        => Sp.Y + Elementary_Functions.Sin (Sp.Rotation) * 10.0,
                                     color     => Color.al_map_rgb (r => 100,
                                                                    g => 100,
                                                                    b => 255),
                                     thickness => 2.0);
      end case;
   end Draw;

   procedure Draw (S : Star) is
   begin
      Bitmap.al_put_blended_pixel (x     => int (S.X),
                                   y     => int (S.Y),
                                   color => Color.al_map_rgba_f (r => 1.0 ,
                                                                 g => 1.0,
                                                                 b => 1.0,
                                                                 a => S.Luminance));
   end Draw;

   procedure Create_Starmap is
   begin
      for I in Starmap'Range loop
         Starmap (I) := Star'(X         => Float (Screen_Width) * Float_Random.Random (Float_RNG),
                              Y         => Float (Screen_Height) * Float_Random.Random (Float_RNG),
                              Vx        => 0.0,
                              Vy        => 0.0,
                              Rotation  => 0.0,
                              Luminance => Float (Screen_Width) * Float_Random.Random (Float_RNG));
      end loop;
   end Create_Starmap;

   procedure Draw_Starmap is
   begin
      for I in Starmap'Range loop
         Starmap (I).Draw;
      end loop;
   end Draw_Starmap;

end Stardust_Engine;
