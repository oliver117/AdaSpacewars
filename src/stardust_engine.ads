with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Calendar;

with Allegro5.Color;
use Allegro5;

package Stardust_Engine is

   Screen_Width : constant := 800;
   Screen_Height : constant := 600;

   type Player_Input is
      record
         Turn_Left : Boolean := False;
         Turn_Right : Boolean := False;
         Accelerate : Boolean := False;
         Decelerate : Boolean := False;
         Fire : Boolean := False;
      end record;

   type Object is abstract tagged
      record
         X : Float;
         Y : Float;
         Vx : Float;
         Vy : Float;
         Rotation : Float; -- TODO: fixed
      end record;

   procedure Draw (Obj : Object) is abstract;

   function Absolute_Velocity (Obj : Object) return Float;

   -- dT : elapsed time
   procedure Move (Obj : in out Object; dT: Duration);

   type Player_Number is (One, Two);

   type Projectile is new Object with record
      C: Color.ALLEGRO_COLOR;
      Shooter : Player_Number;
   end record;

   procedure Draw (P : Projectile);

   package Projectile_Doubly_Linked_Lists is new Ada.Containers.Doubly_Linked_Lists (Projectile);

   type Spaceship is new object with
      record
         Player : Player_Number;
         Hits : Integer := 0;
      end record;

   procedure Draw (Sp : Spaceship);

   type Star is new Object with
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

private

   Float_RNG : Float_Random.Generator;

   Starmap : Star_Array (1 .. 2_500);

   Players : array (Player_Number) of Spaceship;

   Projectile_Register : Projectile_Doubly_Linked_Lists.List;

end Stardust_Engine;
