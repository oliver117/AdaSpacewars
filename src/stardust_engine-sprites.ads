with Ada.Strings.Unbounded;

with Allegro5.Bitmap;
with Allegro5.Color;

with Allegro5.Addon.Font;

package Stardust_Engine.Sprites is

   -- decides where the center of the bitmap shall be blitted.
   type Position_Mode is (Top_Left, Center);

   type Sprite (Pos_Mode : Position_Mode) is new Object_2 and Drawable with
      record
         Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
         Tint : Allegro5.Color.ALLEGRO_COLOR;
         Scale_X : Float := 1.0;
         Scale_Y : Float := 1.0;
      end record;

   procedure Draw (Spr : in Sprite);

   type TextSprite is new Object_2 and Drawable with
      record
         Font : access Allegro5.Addon.Font.ALLEGRO_FONT;
         Text : Ada.Strings.Unbounded.Unbounded_String;
         Color : Allegro5.Color.ALLEGRO_COLOR;
      end record;

   procedure Draw (TSpr : in TextSprite);

end Stardust_Engine.Sprites;
