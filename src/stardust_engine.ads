with Ada.Numerics.Float_Random;
with Interfaces.C; use Interfaces.C;

with Allegro5.Display;
with Allegro5.Events;
with Allegro5.Keycodes;
with Allegro5.Timer;
use Allegro5;

package Stardust_Engine is

   -- Helper subprograms

   type Move_Procedure is access procedure (dT : Float);
   type Render_Procedure is access procedure;

   function Initialize (Width : Integer;
                        Height : Integer;
                        Move_Proc : Move_Procedure;
                        Render_Proc : Render_Procedure) return Boolean;

   procedure Cleanup;

   procedure Start_Timer;

   function Wait_For_Event return Events.ALLEGRO_EVENT;

   procedure Handle_Event (Event : Events.ALLEGRO_EVENT);


   function Get_Screen_Width return int;
   function Get_Screen_Height return int;
   function Get_Display return Display.ALLEGRO_DISPLAY;

   function Want_Close return Boolean;

   function Key_Pressed (Keycode : in Keycodes.ALLEGRO_KEYCODE) return Boolean;

   Wrap_Around : Boolean := True;

private

   Move : access procedure (dT : Float);
   Render : access procedure;

   Screen_Width : int;
   Screen_Height : int;

   Close : Boolean := False;
   Redraw : Boolean := True;

   Disp : Display.ALLEGRO_DISPLAY;
   Event_Queue : Events.ALLEGRO_EVENT_QUEUE;
   Move_Timer : Timer.ALLEGRO_TIMER;

   Float_RNG : Ada.Numerics.Float_Random.Generator;

   Key_Down : array (Keycodes.ALLEGRO_KEYCODE'Range) of Boolean;

end Stardust_Engine;
