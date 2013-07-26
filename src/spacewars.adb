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
with Allegro5.Allegro.Primitives;
with Allegro5.System;
with Allegro5.Timer;
use Allegro5;

with Stardust_Engine;

procedure Spacewars is
   Disp : Display.ALLEGRO_DISPLAY;

   Event_Queue : Events.ALLEGRO_EVENT_QUEUE;

   T : Timer.ALLEGRO_TIMER;
begin
   Ada.Text_IO.Put_Line ("Starting SpaceWars...");

   if System.al_init = 0 then
      Ada.Text_IO.Put_Line ("Failed to initialize system");
   end if;

   if Keyboard.al_install_keyboard = 0 then
      Ada.Text_IO.Put_Line ("Failed to initialize keyboard");
   end if;

   if Allegro.Primitives.al_init_primitives_addon = 0 then
      Ada.Text_IO.Put_Line ("Failed to initialize primitives");
   end if;

   Event_Queue := Events.al_create_event_queue;

   Disp := Display.al_create_display (w => Stardust_Engine.Screen_Width,
                                      h => Stardust_Engine.Screen_Height);

   T := Timer.al_create_timer (1.0 / 60.0); -- 60 fps

   Events.al_register_event_source (Event_Queue, Keyboard.al_get_keyboard_event_source);
   Events.al_register_event_source (Event_Queue, Timer.al_get_timer_event_source (T));
   Events.al_register_event_source (Event_Queue, Display.al_get_display_event_source (Disp));

   Stardust_Engine.Create_Starmap;

   Timer.al_start_timer (T);

   declare
      Event : aliased Events.ALLEGRO_EVENT;

      Player1_Input : Stardust_Engine.Player_Input;
      Player2_Input : Stardust_Engine.Player_Input;

      use Keycodes;

      Redraw : Boolean := True;
   begin
      Main_Loop:
      loop
         Events.al_wait_for_event(Event_Queue, Event'Access);

         if Event.c_type = Events.ALLEGRO_EVENT_DISPLAY_CLOSE then
            goto cleanup;
         end if;

         if Event.c_type = Events.ALLEGRO_EVENT_KEY_DOWN then
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
         elsif Event.c_type = Events.ALLEGRO_EVENT_KEY_UP then
            case Event.keyboard.keycode is
               when ALLEGRO_KEY_A =>
                  Player1_Input.Turn_Left := False;
               when ALLEGRO_KEY_D =>
                  Player1_Input.Turn_Right := False;
               when ALLEGRO_KEY_W =>
                  Player1_Input.Accelerate := False;
               when ALLEGRO_KEY_S =>
                  Player1_Input.Decelerate := False;
               when ALLEGRO_KEY_SPACE =>
                  Player1_Input.Fire := False;

               when ALLEGRO_KEY_LEFT =>
                  Player2_Input.Turn_Left := False;
               when ALLEGRO_KEY_RIGHT =>
                  Player2_Input.Turn_Right := False;
               when ALLEGRO_KEY_UP =>
                  Player2_Input.Accelerate := False;
               when ALLEGRO_KEY_DOWN =>
                  Player2_Input.Decelerate := False;
               when ALLEGRO_KEY_ENTER =>
                  Player2_Input.Fire := False;

               when others =>
                  null;
            end case;

         elsif Event.c_type = Events.ALLEGRO_EVENT_TIMER then
            Stardust_Engine.Move (Player1_Input, Player2_Input, 1.0 / 60.0);
            Player1_Input.Fire := False;
            Player2_Input.Fire := False;
            Redraw := True;

         end if;

         if Events.al_is_event_queue_empty (Event_Queue) /= 0 and Redraw then

            Drawing.al_clear_to_color (Color.al_map_rgb (r => 0,
                                                         g => 0,
                                                         b => 0));

            Stardust_Engine.Draw_Starmap;
            Stardust_Engine.Draw_Players;
            Stardust_Engine.Render_Projectiles;
            Display.al_flip_display;

            Redraw := False;
         end if;

      end loop Main_Loop;
   end;

   <<cleanup>>
   Ada.Text_IO.Put_Line ("Cleaning up ...");

   Display.al_destroy_display (Disp);
   Timer.al_destroy_timer (T);
   Events.al_destroy_event_queue (Event_Queue);
   System.al_uninstall_system;

end Spacewars;
