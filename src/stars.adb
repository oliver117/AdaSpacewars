with Allegro5.Drawing;

package body Stars is

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
