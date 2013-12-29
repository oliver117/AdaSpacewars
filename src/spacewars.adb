with Ada.Text_IO;

with Interfaces.C;

with Allegro5.Events;

use Allegro5;

with Stardust_Engine;
with Stardust_Engine.Entities;


use Stardust_Engine;

procedure Spacewars is

   use type Interfaces.C.unsigned;

   subtype Spaceship is Stardust_Engine.Entities.Entity;

   Player_1 : Spaceship;
   Player_2 : Spaceship;

   procedure Move (dT : Float) is
   begin
      return;
   end Move;

   procedure Render is
   begin
      Entities.Render (Player_1);
      Entities.Render (Player_2);
   end Render;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if not Initialize (800, 600, Move'Unrestricted_Access, Render'Unrestricted_Access) then
      return;
   end if;

   declare
      DB_P1, DB_P2 : Entities.Display_Bitmap;
      Pos_1, Pos_2 : Entities.Position_2D;

   begin
      DB_P1.Load_Bitmap ("../flyer1.png");
      DB_P2.Load_Bitmap ("../flyer2.png");

      Pos_1 := Entities.Position_2D'(Position     => (100.0, 100.0),
                                     Velocity     => (0.0, 0.0),
                                     Acceleration => (0.0, 0.0));
      Pos_2 := Entities.Position_2D'(Position     => (200.0, 200.0),
                                     Velocity     => (0.0, 0.0),
                                     Acceleration => (0.0, 0.0));

      Entities.Add_Component (Player_1, DB_P1);
      Entities.Add_Component (Player_2, DB_P2);

      Entities.Add_Component (Player_1, Pos_1);
      Entities.Add_Component (Player_2, Pos_2);
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
