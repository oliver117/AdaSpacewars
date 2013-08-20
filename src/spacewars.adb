with Ada.Numerics;
with Ada.Text_IO;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;

with Allegro5.Bitmap;
with Allegro5.Bitmap_IO;
with Allegro5.Color;
with Allegro5.Events;
with Allegro5.Keycodes;  use Allegro5;

with Stardust_Engine;         use Stardust_Engine;
with Stardust_Engine.Sprites;
with Stardust_Engine.Stars;

procedure Spacewars is

   type Spaceship is new Sprites.Sprite (Sprites.Center) with null record;

   overriding
   procedure Move (Sp : in out Spaceship; dT : in Duration) is
   begin
      if Key_Pressed (Keycodes.ALLEGRO_KEY_W) then
         Set_Speed (Sp.Vel, Speed (Sp.Vel) + Float (1.0 * dT));
      end if;

      if Key_Pressed (Keycodes.ALLEGRO_KEY_S) then
         Set_Speed (Sp.Vel, Speed (Sp.Vel) - Float (1.0 * dT));
      end if;

      if Key_Pressed (Keycodes.ALLEGRO_KEY_A) then
         Sp.Orientation := Sp.Orientation + Float (1.0 * dT);
         Set_Direction (Sp.Vel, Sp.Orientation);
      end if;

      if Key_Pressed (Keycodes.ALLEGRO_KEY_D) then
         Sp.Orientation := Sp.Orientation - Float (1.0 * dT);
         Set_Direction (Sp.Vel, Sp.Orientation);
      end if;
   end Move;

   Player_1 : aliased Spaceship;
   Player_2 : aliased Spaceship;

   Star_Background : Sprites.Sprite (Sprites.Top_Left);
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if not Initialize (800, 600) then
      return;
   end if;

   Player_1 :=
     Spaceship'
     (Pos         => Position_2'(0.0, 0.0),
      Orientation => 0.0,
      Pos_Mode    => Sprites.Center,
      Bitmap      => Bitmap_IO.al_load_bitmap
                       (Interfaces.C.Strings.New_String ("../flyer1.png")),
      Tint        => Color.al_map_rgb_f (r => 1.0, g => 0.5, b => 0.5),
      Scale_X     => 0.05,
      Scale_Y     => 0.05,
      others => <>);
   Player_2 :=
     Spaceship'
     (Pos         => Position_2'(0.0, 0.0),
      Orientation => 0.0,
      Pos_Mode    => Sprites.Center,
      Bitmap      => Bitmap_IO.al_load_bitmap
                       (Interfaces.C.Strings.New_String ("../flyer2.png")),
      Tint        => Color.al_map_rgb_f (r => 0.5, g => 0.5, b => 1.0),
      Scale_X     => 0.05,
      Scale_Y     => 0.05,
      others => <>);

   Star_Background :=
     Sprites.Sprite'
     (Pos         => Position_2'(0.0, 0.0),
      Orientation => 0.0,
      Pos_Mode    => Sprites.Top_Left,
      Bitmap      => Allegro5.Bitmap.al_create_bitmap
                       (w => Get_Screen_Width,
                        h => Get_Screen_Height),
      Tint        => Color.al_map_rgb_f (r => 1.0, g => 1.0, b => 1.0),
      others => <>);

   Stars.Draw_On_Bitmap (Star_Background.Bitmap, 2500);

   Object_List.Append (Player_1);
   -- Object_List.Append (Player_2);

   Star_Timer;

   declare
      Event : Events.ALLEGRO_EVENT;
   begin
      Main_Loop : loop
         Event := Wait_For_Event;
         if Event.c_type = Events.ALLEGRO_EVENT_KEY_DOWN then
            if Event.keyboard.keycode = Keycodes.ALLEGRO_KEY_RIGHT then
               Player_1.Orientation := Player_1.Orientation + 0.01 * Ada.Numerics.Pi;
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
