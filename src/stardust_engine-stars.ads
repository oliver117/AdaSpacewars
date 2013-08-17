with Allegro5.Bitmap;
with Allegro5.Color;
with Stardust_Engine; use Stardust_Engine;

package Stardust_Engine.Stars is
   type Star is new Drawable with
      record
         Pos : Position_2;
         Color : Allegro5.Color.ALLEGRO_COLOR;
      end record;

   type Star_Array is array (Integer range <>) of Star;

   type Star_Map (n : Positive) is new Drawable with
      record
         Stars : Star_Array (1 .. n);
      end record;

   -- randomizes the positions and colors of the stars in the starmap
   procedure Random_Stars (SM : in out Star_Map; Max_X : in Float; Max_Y : in Float);

   procedure Draw (S : in Star);
   procedure Draw (SM : in Star_Map);

   -- creates some random stars and draws them on a bitmap
   procedure Draw_On_Bitmap (Bitmap : in out Allegro5.Bitmap.ALLEGRO_BITMAP; Stars : in Integer);

end Stardust_Engine.Stars;
