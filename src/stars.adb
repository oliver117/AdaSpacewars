with Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Numerics.Float_Random;

with Allegro5.Drawing;

package body Stars is

   procedure Random_Stars (SM : in out Star_Map; Max_X : in Float; Max_Y : in Float) is
      use Ada.Calendar;
      use Ada.Numerics.Float_Random;
      Gen : Generator;
   begin
      Reset (Gen, Integer (Seconds (Clock)));
      for I in SM.Stars'Range loop
         SM.Stars (I).Pos.X := Random (Gen) * Max_X;
         SM.Stars (I).Pos.X := Random (Gen) * Max_Y;

         SM.Stars (I).Color := Allegro5.Color.al_map_rgb_f (r => Random (Gen),
                                                            g => Random (Gen),
                                                            b => Random (Gen));
      end loop;

   end Random_Stars;

   procedure Draw (S : in Star) is
   begin
      Allegro5.Drawing.al_draw_pixel (x     => S.Pos.X,
                                      y     => S.Pos.Y,
                                      color => S.Color);
   end Draw;

   procedure Draw (SM : in Star_Map) is
   begin
      for I in SM.Stars'Range loop
         SM.Stars (I).Draw;
      end loop;
   end Draw;

end Stars;
