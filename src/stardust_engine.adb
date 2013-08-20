with Ada.Numerics.Elementary_Functions; use Ada.Numerics;
with Ada.Text_IO;

with Allegro5.Color;
with Allegro5.Drawing;
with Allegro5.Keyboard;
with Allegro5.System;

with Allegro5.Addon.Image;
with Allegro5.Addon.Primitives;

use Allegro5.Addon;

package body Stardust_Engine is

   procedure Draw is
      procedure Each (Cur : Object_Lists.Cursor) is
         Obj : Object'Class := Object_Lists.Element (Cur);
      begin
         if Obj in Drawable'Class then
            Drawable'Class (Obj).Draw;
         end if;
      end Each;
   begin
      -- TODO: does it make sense to clear the screen to black?
      Drawing.al_clear_to_color (Color.al_map_rgb (r => 0,
                                                   g => 0,
                                                   b => 0));

      Object_List.Iterate (Each'Access);

      Display.al_flip_display;
   end Draw;

   procedure Move (dT : Duration) is
      procedure Each (Obj : in out Object'Class) is
      begin
         if Obj in Movable'Class then
            Movable'Class (Obj).Move(dT);
         end if;
      end Each;

      Cur : Object_Lists.Cursor := Object_List.First;
   begin
      while Object_Lists.Has_Element (Cur) loop
         Object_Lists.Update_Element (Object_List, Cur, Each'Access);
         Object_Lists.Next (Cur);
      end loop;
   end Move;

   overriding
   procedure Move (Obj : in out Object_2; dT : Duration) is
   begin
      Obj.Pos.X := Obj.Pos.X + Obj.Vel.X * Float (dT) + 0.5 * Obj.Acc.X * Float (dT) ** 2;
      Obj.Pos.Y := Obj.Pos.Y + Obj.Vel.Y * Float (dT) + 0.5 * Obj.Acc.X * Float (dT) ** 2;
   end Move;

   function Direction (Vec : in Vector_2) return Float is
     (Elementary_Functions.Arctan (Vec.X, Vec.Y));

   function Speed (Vec : in Vector_2) return Float is
     (Elementary_Functions.Sqrt (Vec.X ** 2 + Vec.Y ** 2));

   procedure Set_Direction (Vec : in out Vector_2; Dir : in Float) is
      Sp : constant Float := Speed (Vec);
   begin
      Vec.X := Elementary_Functions.Cos (Dir) * Sp;
      Vec.Y := Elementary_Functions.Sin (Dir) * Sp;
   end Set_Direction;

   procedure Set_Speed (Vec : in out Vector_2; Sp : in Float) is
      Dir : constant Float := Direction (Vec);
   begin
      Vec.X := Elementary_Functions.Cos (Dir) * Sp;
      Vec.Y := Elementary_Functions.Sin (Dir) * Sp;
   end Set_Speed;

   function Initialize (Width : Integer;
                        Height : Integer) return Boolean is
      Success : Boolean := True;
   begin
      Ada.Text_IO.Put ("System ... ");
      if System.al_init = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         goto deinit_system;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Ada.Text_IO.Put ("Keyboard ... ");
      if Keyboard.al_install_keyboard = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         Success := False;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Ada.Text_IO.Put ("Primitives ... ");
      if Addon.Primitives.al_init_primitives_addon = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         Success := False;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Ada.Text_IO.Put ("Image ... ");
      if Image.al_init_image_addon = 0 then
         Ada.Text_IO.Put_Line ("FAIL");
         Success := False;
      else
         Ada.Text_IO.Put_Line ("OK");
      end if;

      Event_Queue := Events.al_create_event_queue;

      Screen_Width := int (Width);
      Screen_Height := int (Height);

      Disp := Display.al_create_display (w => Screen_Width,
                                         h => Screen_Height);

      Move_Timer := Timer.al_create_timer (1.0 / 60.0); -- 60 fps

      Events.al_register_event_source (Event_Queue, Keyboard.al_get_keyboard_event_source);
      Events.al_register_event_source (Event_Queue, Timer.al_get_timer_event_source (Move_Timer));
      Events.al_register_event_source (Event_Queue, Display.al_get_display_event_source (Disp));

      return Success;

      <<deinit_system>>
      System.al_uninstall_system;
      return False;
   end Initialize;

   procedure Cleanup is
   begin
      Display.al_destroy_display (Disp);
      Timer.al_destroy_timer (Move_Timer);
      Events.al_destroy_event_queue (Event_Queue);
      System.al_uninstall_system;
   end Cleanup;

   procedure Star_Timer is
   begin
      Timer.al_start_timer (Move_Timer);
   end Star_Timer;

   function Wait_For_Event return Events.ALLEGRO_EVENT is
      Event : aliased Events.ALLEGRO_EVENT;
   begin
      Events.al_wait_for_event (Event_Queue, Event'Access);
      return Event;
   end Wait_For_Event;

   procedure Handle_Event (Event : Events.ALLEGRO_EVENT) is
   begin
      if Event.c_type = Events.ALLEGRO_EVENT_DISPLAY_CLOSE then
         Close := True;
      elsif Event.c_type = Events.ALLEGRO_EVENT_KEY_DOWN and then
        Event.keyboard.keycode = Keycodes.ALLEGRO_KEY_ESCAPE then
         Close := True;
      elsif Event.c_type = Events.ALLEGRO_EVENT_KEY_DOWN then
         Key_Down (Event.keyboard.keycode) := True;
      elsif Event.c_type = Events.ALLEGRO_EVENT_KEY_UP then
         Key_Down (Event.keyboard.keycode) := False;
      elsif Event.c_type = Events.ALLEGRO_EVENT_TIMER then
         Move (1.0 / 60.0);
         Redraw := True;
      end if;

      if Events.al_is_event_queue_empty (Event_Queue) /= 0 and Redraw then
         Draw;
         Redraw := False;
      end if;
   end Handle_Event;

   function Get_Screen_Width return int is (Screen_Width);

   function Get_Screen_Height return int is (Screen_Height);

   function Get_Display return Display.ALLEGRO_DISPLAY is (Disp);

   function Want_Close return Boolean is (Close);

   function Key_Pressed (Keycode : in Keycodes.ALLEGRO_KEYCODE) return Boolean is
      (Key_Down (Keycode));

end Stardust_Engine;
