with Ada.Numerics.Elementary_Functions; use Ada.Numerics;
with Ada.Numerics.Float_Random;
with Ada.Calendar;

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
   function Move (Obj : Object'Class; dT: Duration) return Object'Class;

   procedure Move (Obj : in out Object'Class; dT: Duration);

   type Player_Number is (One, Two);

   type Spaceship is new object with
      record
         Player : Player_Number;
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

private

   Float_RNG : Float_Random.Generator;

   Starmap : Star_Array (1 .. 2_500);

end Stardust_Engine;
