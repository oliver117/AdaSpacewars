with Interfaces.C.Strings;

with Allegro5.Bitmap_Draw;

package body Stardust_Engine.Sprites is

   procedure Draw (Spr : in Sprite) is
      dx, dy : Float;
   begin
      dx := Spr.Pos.X;
      dy := Spr.Pos.Y;

      if Spr.Pos_Mode = Center then
         dx := dx + Float (Allegro5.Bitmap.al_get_bitmap_width (Spr.Bitmap)) / 2.0;
         dy := dy + Float (Allegro5.Bitmap.al_get_bitmap_height (Spr.Bitmap)) / 2.0;
         -- TODO: accomodate for scaling
      end if;

      Allegro5.Bitmap_Draw.al_draw_tinted_scaled_rotated_bitmap (bitmap => Spr.Bitmap,
                                                                 tint   => Spr.Tint,
                                                                 cx     => 0.0,
                                                                 cy     => 0.0,
                                                                 dx     => dx,
                                                                 dy     => dy,
                                                                 xscale => Spr.Scale_X,
                                                                 yscale => Spr.Scale_Y,
                                                                 angle  => Spr.Orientation,
                                                                 flags  => 0);
   end Draw;

   procedure Draw (TSpr : in TextSprite) is
   begin
      Allegro5.Addon.Font.al_draw_text (font  => TSpr.Font,
                                        color => TSpr.Color,
                                        x     => TSpr.Pos.X,
                                        y     => TSpr.Pos.Y,
                                        flags => 0,
                                        text  => Interfaces.C.Strings.New_String (
                                          Ada.Strings.Unbounded.To_String (TSpr.Text)));
                                        end Draw;


end Stardust_Engine.Sprites;
