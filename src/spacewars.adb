with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;


with Allegro5.Base;
with Allegro5.Color;
with Allegro5.Display;
with Allegro5.Drawing;
with Allegro5.Error;
with Allegro5.Events;
with Allegro5.Keyboard;
with Allegro5.Keycodes;
with Allegro5.System;
use Allegro5;

with Stardust_Engine;

procedure Spacewars is
   Disp : Display.ALLEGRO_DISPLAY;

   Event_Queue : Events.ALLEGRO_EVENT_QUEUE;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if System.al_init > 0 then
      Ada.Text_IO.Put_Line ("Failed to initialize system");
      Ada.Text_IO.Put_Line (int'Image (Error.al_get_errno));
   end if;

   if Keyboard.al_install_keyboard > 0 then
      Ada.Text_IO.Put_Line ("Failed to initialize keyboard");
   end if;

   Event_Queue := Events.al_create_event_queue;

   Disp := Display.al_create_display (w => Stardust_Engine.Screen_Width,
                                      h => Stardust_Engine.Screen_Height);

   Events.al_register_event_source(Event_Queue, Keyboard.al_get_keyboard_event_source);

   Stardust_Engine.Create_Starmap;

   declare
      Event : aliased Events.ALLEGRO_EVENT;

      Player1_Input : Stardust_Engine.Player_Input;
      Player2_Input : Stardust_Engine.Player_Input;

      use Keycodes;
   begin
      Main_Loop:
      loop
         Events.al_wait_for_event(Event_Queue, Event'Access);

         Player1_Input := Stardust_Engine.Player_Input'(others => False);
         Player2_Input := Player1_Input;

         if Event.c_type = 10 then
            case Event.keyboard.keycode is
               when ALLEGRO_KEY_ESCAPE =>
                  goto cleanup;

               when ALLEGRO_KEY_A =>
                  Player1_Input.Turn_Left := True;
               when ALLEGRO_KEY_D =>
                  Player1_Input.Turn_Right := True;
               when ALLEGRO_KEY_W =>
                  Player1_Input.Accelerate := True;
               when ALLEGRO_KEY_S =>
                  Player1_Input.Decelerate := True;
               when ALLEGRO_KEY_SPACE =>
                  Player1_Input.Fire := True;

               when ALLEGRO_KEY_LEFT =>
                  Player2_Input.Turn_Left := True;
               when ALLEGRO_KEY_RIGHT =>
                  Player2_Input.Turn_Right := True;
               when ALLEGRO_KEY_UP =>
                  Player2_Input.Accelerate := True;
               when ALLEGRO_KEY_DOWN =>
                  Player2_Input.Decelerate := True;
               when ALLEGRO_KEY_ENTER =>
                  Player2_Input.Fire := True;

               when others =>
                  null;
            end case;
         end if;

         Drawing.al_clear_to_color (Color.al_map_rgb (r => 0,
                                                      g => 0,
                                                      b => 0));

         Stardust_Engine.Draw_Starmap;
      end loop Main_Loop;
   end;

   <<cleanup>>
   Ada.Text_IO.Put_Line ("Cleaning up ...");

   Display.al_destroy_display (Disp);
   Events.al_destroy_event_queue (Event_Queue);
   System.al_uninstall_system;

end Spacewars;
