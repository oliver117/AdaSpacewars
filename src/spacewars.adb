with Ada.Text_IO;

with Interfaces.C;

with Allegro5.Color;
with Allegro5.Drawing;
with Allegro5.Events;
with Allegro5.Keycodes;

use Allegro5;

with Stardust_Engine;
with Stardust_Engine.Entities;
with Stardust_Engine.Entities.Components;


use Stardust_Engine;

procedure Spacewars is

   use type Interfaces.C.unsigned;

   subtype Spaceship is Stardust_Engine.Entities.Entity;

   type Starmap is array (Integer range <>) of Entities.Entity;

   Player_1 : Spaceship;
   Player_2 : Spaceship;

   Stars : Starmap (1 .. 1000);

   procedure Move (dT : Float) is
      use Entities;

      package TC_Access is new Stardust_Engine.Entities.Components (Component_Type => Turn_Control);
      package AC_Access is new Stardust_Engine.Entities.Components (Component_Type => Acceleration_Control);

   begin
      Process (Turn_Control (TC_Access.First_Component (Player_1)), Player_1);
      Process (Turn_Control (TC_Access.First_Component (Player_2)), Player_2);

      Process (Acceleration_Control (AC_Access.First_Component (Player_1)), Player_1);
      Process (Acceleration_Control (AC_Access.First_Component (Player_2)), Player_2);

      Move (Player_1, dT);
      Move (Player_2, dT);
   end Move;

   procedure Render is
   begin
      Drawing.al_clear_to_color (Color.al_map_rgb_f (0.0, 0.0, 0.0));

      Entities.Render (Player_1);
      Entities.Render (Player_2);

      for I in Stars'Range loop
         Entities.Render (Stars (I));
      end loop;
   end Render;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if not Initialize (800, 600, Move'Unrestricted_Access, Render'Unrestricted_Access) then
      return;
   end if;

   begin
      for I in Stars'Range loop
         Entities.Add_Component (Stars (I), Entities.Position_2D'(Position     => (0.0, 0.0),
                                                          Velocity     => (0.0, 0.0),
                                                          Acceleration => (0.0, 0.0)));
         Entities.Add_Component (Stars (I), Entities.Pixel'(Color => Color.al_map_rgb (255, 255, 255)));
      end loop;
   end;

   declare
      B_P1, B_P2 : Entities.Tinted_Scaled_Rotated_Bitmap;
      Pos_1, Pos_2 : Entities.Position_2D;
      Rot_1, Rot_2 : Entities.Rotation;

      TC_1 : Entities.Turn_Control := (Turn_Left  => Keycodes.ALLEGRO_KEY_LEFT,
                                       Turn_Right => Keycodes.ALLEGRO_KEY_RIGHT,
                                       Rate       => 2.5);
      TC_2 : Entities.Turn_Control := (Turn_Left  => Keycodes.ALLEGRO_KEY_A,
                                       Turn_Right => Keycodes.ALLEGRO_KEY_D,
                                       Rate       => 2.5);

      AC_1 : Entities.Acceleration_Control := (Accelerate => Keycodes.ALLEGRO_KEY_UP,
                                               Decelerate => Keycodes.ALLEGRO_KEY_DOWN,
                                               Rate       => 250.0);

      AC_2 : Entities.Acceleration_Control := (Accelerate => Keycodes.ALLEGRO_KEY_W,
                                               Decelerate => Keycodes.ALLEGRO_KEY_S,
                                               Rate       => 250.0);

   begin
      Entities.Load_Bitmap (Entities.Bitmap'Class (B_P1), "../flyer1.png");
      Entities.Load_Bitmap (Entities.Bitmap'Class(B_P2), "../flyer2.png");

      B_P1.Scale.X := 0.1;
      B_P1.Scale.Y := 0.1;
      B_P1.Tint := Color.al_map_rgb (100, 100, 255);

      B_P2.Scale.X := 0.1;
      B_P2.Scale.Y := 0.1;
      B_P2.Tint := Color.al_map_rgb (255, 100, 100);

      Pos_1 := Entities.Position_2D'(Position     => (100.0, 100.0),
                                     Velocity     => (0.0, 0.0),
                                     Acceleration => (0.0, 0.0));
      Pos_2 := Entities.Position_2D'(Position     => (200.0, 200.0),
                                     Velocity     => (0.0, 0.0),
                                     Acceleration => (0.0, 0.0));

      Entities.Add_Component (Player_1, B_P1);
      Entities.Add_Component (Player_2, B_P2);

      Entities.Add_Component (Player_1, Pos_1);
      Entities.Add_Component (Player_2, Pos_2);

      Entities.Add_Component (Player_1, Rot_1);
      Entities.Add_Component (Player_2, Rot_2);

      Entities.Add_Component (Player_1, TC_1);
      Entities.Add_Component (Player_2, TC_2);

      Entities.Add_Component (Player_1, AC_1);
      Entities.Add_Component (Player_2, AC_2);
   end;

   Start_Timer;

   declare
      Event : Events.ALLEGRO_EVENT;
   begin
      Main_Loop : loop
         Event := Wait_For_Event;

         Handle_Event (Event);

         if Want_Close then
            goto cleanup;
         end if;

      end loop Main_Loop;
   end;

   <<cleanup>>
   Stardust_Engine.Cleanup;

end Spacewars;
