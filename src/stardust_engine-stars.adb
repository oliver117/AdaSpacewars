with Ada.Calendar;
with Ada.Numerics.Float_Random;

with Allegro5.Display;
with Allegro5.Drawing;

package body Stardust_Engine.Stars is

   procedure Random_Stars (SM : in out Star_Map; Max_X : in Float; Max_Y : in Float) is
      use Ada.Calendar;
      use Ada.Numerics.Float_Random;
      Gen : Generator;

      Luminance : Float;
   begin
      -- initialize the RNG / seconds of the current day
      Reset (Gen, Integer (Seconds (Clock)));

      for I in SM.Stars'Range loop
         SM.Stars (I).Pos.X := Random (Gen) * Max_X;
         SM.Stars (I).Pos.Y := Random (Gen) * Max_Y;

         Luminance := Random (Gen) * 0.3 + 0.7;

         SM.Stars (I).Color := Allegro5.Color.al_map_rgb_f (r => Luminance,
                                                            g => Luminance,
                                                            b => Luminance);
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

   procedure Draw_On_Bitmap (Bitmap : in out Allegro5.Bitmap.ALLEGRO_BITMAP; Stars : in Integer) is
      use Allegro5.Bitmap;

      SM : Star_Map (Stars);
      Width : Integer;
      Height : Integer;
   begin
      Width := Integer (al_get_bitmap_width (Bitmap));
      Height := Integer (al_get_bitmap_height (Bitmap));

      -- create some random stars
      Random_Stars (SM, Float(Width), Float(Height));

      -- draw on the bitmap
      Allegro5.Display.al_set_target_bitmap (Bitmap);

      -- nice black background
      Allegro5.Drawing.al_clear_to_color (Allegro5.Color.al_map_rgb (r => 0,
                                                                     g => 0,
                                                                     b => 0));
      SM.Draw;
      Allegro5.Display.al_set_target_backbuffer (Stardust_Engine.Get_Display);
   end Draw_On_Bitmap;

end Stardust_Engine.Stars;
