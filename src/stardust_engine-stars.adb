with Ada.Numerics.Float_Random;
with Allegro5.Color;
with Allegro5.Display;
with Allegro5.Drawing;

package body Stardust_Engine.Stars is

   ----------------
   -- Init_Stars --
   ----------------

   procedure Init_Stars
     (Stars : in out Star_Array;
      Min, Max : in Entities.Vector_2D)
   is
      use Ada.Numerics;

      RNG : Float_Random.Generator;
   begin
      Float_Random.Reset (RNG);
      for I in Stars'Range loop
         Stars (I).X := Min.X + Float_Random.Random (RNG) * (Max.X - Min.X);
         Stars (I).Y := Min.Y + Float_Random.Random (RNG) * (Max.Y - Min.Y);
      end loop;
   end Init_Stars;

   ------------
   -- Render --
   ------------

   procedure Render (Stars : in Star_Array) is
   begin
      for I in Stars'Range loop
         Drawing.al_draw_pixel (x     => Stars (I).X,
                                y     => Stars (I).Y,
                                color => Color.al_map_rgb_f (0.5, 0.5, 0.5));
      end loop;
   end Render;


   ----------------------
   -- Render_To_Bitmap --
   ----------------------

   procedure Render_To_Bitmap
     (Stars : in Star_Array;
      B : in out Allegro5.Bitmap.ALLEGRO_BITMAP)
   is
      -- save current/display
      D : constant Display.ALLEGRO_DISPLAY := Display.al_get_current_display;
   begin
      -- draw on 'B'
      Display.al_set_target_bitmap (B);

      Render (Stars);

      -- reset display
      Display.al_set_target_backbuffer (D);
   end Render_To_Bitmap;

end Stardust_Engine.Stars;
