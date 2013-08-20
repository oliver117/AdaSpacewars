with Ada.Numerics.Float_Random;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Interfaces.C; use Interfaces.C;

with Allegro5.Display;
with Allegro5.Events;
with Allegro5.Keycodes;
with Allegro5.Timer;
use Allegro5;

package Stardust_Engine is

   -- Abstract
   type Object is interface;

   type Drawable is interface and Object;
   type Movable is interface and Object;

   procedure Draw (D : Drawable) is abstract;
   procedure Move (M : in out Movable; dT : Duration) is abstract;

   package Object_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class);

   Object_List : Object_Lists.List := Object_Lists.Empty_List;
   -- TODO: make that a tree/multilist?, Z-index?

   procedure Draw; -- Draws all drawables in the object list.

   -- Moves all movables in the object list.
   procedure Move (dT : Duration); -- dT : elapsed time

   -- Concrete
   -- TODO: Vector_2 tagged?
   type Vector_2 is
      record
         X : Float;
         Y : Float;
      end record;

   Null_Vector_2 : constant Vector_2 := Vector_2'(0.0, 0.0);

   function Direction (Vec : in Vector_2) return Float;
   function Speed (Vec : in Vector_2) return Float;

   procedure Set_Direction (Vec : in out Vector_2; Dir : in Float) with
     Post => Direction (Vec) = Dir;

   procedure Set_Speed (Vec : in out Vector_2; Sp : in Float) with
     Post => Speed (Vec) = Sp;

   type Position_2 is new Vector_2;
   type Velocity_2 is new Vector_2;
   type Acceleration_2 is new Vector_2;

   -- TODO: better name
   type Object_2 is abstract new Movable with
      record
         Pos : Position_2;
         Orientation : Float := 0.0;
         Vel : Velocity_2 := Velocity_2'(0.0, 0.0);
         Acc : Acceleration_2 := Acceleration_2'(0.0, 0.0);
      end record;

   overriding
   procedure Move (Obj : in out Object_2; dT : Duration);


   -- Helper subprograms

   function Initialize (Width : Integer;
                        Height : Integer) return Boolean;

   procedure Cleanup;

   procedure Star_Timer;

   function Wait_For_Event return Events.ALLEGRO_EVENT;

   procedure Handle_Event (Event : Events.ALLEGRO_EVENT);


   function Get_Screen_Width return int;
   function Get_Screen_Height return int;
   function Get_Display return Display.ALLEGRO_DISPLAY;

   function Want_Close return Boolean;

   function Key_Pressed (Keycode : in Keycodes.ALLEGRO_KEYCODE) return Boolean;

private

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
