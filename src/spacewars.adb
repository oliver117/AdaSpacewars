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
with Stardust_Engine.Stars;


use Stardust_Engine;

procedure Spacewars is
   use type Interfaces.C.unsigned;

   subtype Spaceship is Entities.Entity;

   Player_1 : Spaceship;
   Player_2 : Spaceship;

   Starmap : Stars.Star_Array (1 .. 2500);

   procedure Move (dT : Float) is
      use Entities;

      package TC_Access is new Components (Component_Type => Turn_Control);
      package AC_Access is new Components (Component_Type => Acceleration_Control);

   begin
      Process (Turn_Control (TC_Access.First_Component (Player_1)), Player_1);
      Process (Turn_Control (TC_Access.First_Component (Player_2)), Player_2);

      Process (Acceleration_Control (AC_Access.First_Component (Player_1)), Player_1);
      Process (Acceleration_Control (AC_Access.First_Component (Player_2)), Player_2);

      Move (Player_1, dT);
      Move (Player_2, dT);
   end Move;

   procedure Render is
      use Entities;
   begin
      Drawing.al_clear_to_color (Color.al_map_rgb_f (0.0, 0.0, 0.0));
      Stars.Render (Starmap);

      Render (Player_1);
      Render (Player_2);
   end Render;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if not Initialize (800, 600, Move'Unrestricted_Access, Render'Unrestricted_Access) then
      return;
   end if;

   Stars.Init_Stars (Stars => Starmap,
                     Min   => (0.0, 0.0),
                     Max   => (800.0, 600.0));

   declare
      use Entities;

      B_P1, B_P2 : Tinted_Scaled_Rotated_Bitmap;
      Pos_1, Pos_2 : Position_2D;
      Rot_1, Rot_2 : Rotation;

      TC_1 : constant Turn_Control := (Turn_Left  => Keycodes.ALLEGRO_KEY_LEFT,
                                       Turn_Right => Keycodes.ALLEGRO_KEY_RIGHT,
                                       Rate       => 2.5);
      TC_2 : constant Turn_Control := (Turn_Left  => Keycodes.ALLEGRO_KEY_A,
                                       Turn_Right => Keycodes.ALLEGRO_KEY_D,
                                       Rate       => 2.5);

      AC_1 : constant Acceleration_Control := (Accelerate => Keycodes.ALLEGRO_KEY_UP,
                                               Decelerate => Keycodes.ALLEGRO_KEY_DOWN,
                                               Rate       => 250.0);
      AC_2 : constant Acceleration_Control := (Accelerate => Keycodes.ALLEGRO_KEY_W,
                                               Decelerate => Keycodes.ALLEGRO_KEY_S,
                                               Rate       => 250.0);

   begin
      Load_Bitmap (Bitmap'Class (B_P1), "../flyer1.png");
      Load_Bitmap (Bitmap'Class(B_P2), "../flyer2.png");

      B_P1.Scale.X := 0.1;
      B_P1.Scale.Y := 0.1;
      B_P1.Tint := Color.al_map_rgb (100, 100, 255);

      B_P2.Scale.X := 0.1;
      B_P2.Scale.Y := 0.1;
      B_P2.Tint := Color.al_map_rgb (255, 100, 100);

      Pos_1 := Position_2D'(Position     => (100.0, 100.0),
                            Velocity     => (0.0, 0.0),
                            Acceleration => (0.0, 0.0));
      Pos_2 := Position_2D'(Position     => (200.0, 200.0),
                            Velocity     => (0.0, 0.0),
                            Acceleration => (0.0, 0.0));

      Add_Component (Player_1, B_P1);
      Add_Component (Player_2, B_P2);

      Add_Component (Player_1, Pos_1);
      Add_Component (Player_2, Pos_2);

      Add_Component (Player_1, Rot_1);
      Add_Component (Player_2, Rot_2);

      Add_Component (Player_1, TC_1);
      Add_Component (Player_2, TC_2);

      Add_Component (Player_1, AC_1);
      Add_Component (Player_2, AC_2);
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
