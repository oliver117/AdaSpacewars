with Ada.Numerics.Elementary_Functions; use Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Calendar;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Interfaces.C; use Interfaces.C;

with Allegro5.Color;
with Allegro5.Display;
with Allegro5.Events;
with Allegro5.Timer;
use Allegro5;

package Stardust_Engine is

   -- Abstract
   type Object is interface;
   type Drawable is interface and Object;
   type Movable is interface and Object;

   procedure Draw (D : Drawable) is abstract;
   procedure Move (M : in out Movable; dT : Duration) is abstract;

   package Object_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class);

   Object_List : Object_Lists.List := Object_Lists.Empty_List;

   procedure Draw; -- Draws all drawables in the object list.
   procedure Move (dT : Duration); -- Moves all movables in the object list.

   -- Concrete
   type Position_2 is
      record
         X : Float;
         Y : Float;
      end record;

   type Velocity_2 is
      record
         Vx : Float;
         Vy : Float;
      end record;

   type Object_2 is abstract new Movable with
      record
         Pos : Position_2;
         Vel : Velocity_2;
      end record;

   overriding
   procedure Move (Obj : in out Object_2; dT : Duration);


   -- Helper subprograms

   function Initialize (Width : Integer;
                        Height : Integer) return Boolean;

   procedure Cleanup;

   procedure Star_Timer;

   procedure Event_Loop;


   type Player_Input is
      record
         Turn_Left : Boolean := False;
         Turn_Right : Boolean := False;
         Accelerate : Boolean := False;
         Decelerate : Boolean := False;
         Fire : Boolean := False;
      end record;

   type Object_X is abstract tagged
      record
         X : Float;
         Y : Float;
         Vx : Float;
         Vy : Float;
         Rotation : Float; -- TODO: fixed
      end record;

   procedure Draw (Obj : Object_X) is abstract;

   function Absolute_Velocity (Obj : Object_X) return Float;

   -- dT : elapsed time
   procedure Move (Obj : in out Object_X; dT: Duration);

   type Player_Number is (One, Two);

   type Projectile is new Object_X with record
      C: Color.ALLEGRO_COLOR;
      Shooter : Player_Number;
   end record;

   procedure Draw (P : Projectile);

   package Projectile_Doubly_Linked_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Projectile);

   type Spaceship is new Object_X with
      record
         Player : Player_Number;
         Hits : Integer := 0;
      end record;

   procedure Draw (Sp : Spaceship);

   type Star is new Object_X with
      record
         Luminance: Float; -- range
      end record;

   procedure Draw (S : Star);

   type Star_Array is array (Integer range <>) of Star;

   procedure Create_Starmap;
   procedure Draw_Starmap;

   procedure Draw_Players;

   Procedure Move (PI_1 : Player_Input; PI_2 : Player_Input; dT: Float);

   procedure Render_Projectiles;

   function Get_Screen_Width return int;
   function Get_Screen_Height return int;

   function Want_Close return Boolean;

private

   Screen_Width : int;
   Screen_Height : int;

   Close : Boolean := False;
   Redraw : Boolean := True;

   Disp : Display.ALLEGRO_DISPLAY;
   Event_Queue : Events.ALLEGRO_EVENT_QUEUE;
   Move_Timer : Timer.ALLEGRO_TIMER;

   Float_RNG : Float_Random.Generator;

   Starmap : Star_Array (1 .. 2_500);

   Players : array (Player_Number) of Spaceship;

   Projectile_Register : Projectile_Doubly_Linked_Lists.List;

end Stardust_Engine;
