with Allegro5.Bitmap;
with Stardust_Engine.Entities;

package Stardust_Engine.Stars is
   subtype Star is Entities.Vector_2D;
   type Star_Array is array (Integer range <>) of Star;

   procedure Init_Stars (Stars : in out Star_Array; Min, Max : in Entities.Vector_2D);

   procedure Render (Stars : in Star_Array);

   procedure Render_To_Bitmap (Stars : in Star_Array; B : in out Allegro5.Bitmap.ALLEGRO_BITMAP);
end Stardust_Engine.Stars;
