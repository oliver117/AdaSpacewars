with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Allegro5.Bitmap;
with Allegro5.Color;

package Stardust_Engine.Entities is

   -- General

   type Vector_2D is record
      X, Y : Float;
   end record;

   -- Component

   type Component is interface;

   type Null_Component_T is new Component with null record;

   Null_Component : constant Null_Component_T := Null_Component_T'(others => <>);

   -- Entity

   package Component_Lists is new
      Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type => Component'Class);

   type Entity is limited private;

   function New_Entity return Entity;

   procedure Reset_Entity (E : in out Entity);

   procedure Add_Component (E : in out Entity; C : in Component'Class);

   function Has_Components (E : Entity) return Boolean;

   function Count_Components (E : Entity) return Natural;

   procedure Render (E : in Entity);

   -- Position_2D

   type Position_2D is new Component with record
      Position : Vector_2D;
      Velocity : Vector_2D;
      Acceleration : Vector_2D;
   end record;

   -- Rotation

   type Rotation is new Component with record
      Angle : Float := 0.0;
      Angular_Velocity : Float := 0.0;
      Angular_Acceleration : Float := 0.0;
   end record;

   -- Move

   procedure Move (E : in out Entity; dT : in Float);

   -- Display

   type Display is interface and Component;

   procedure Render (D : in Display; E : in Entity) is abstract;

   type Pixel is new Display with record
      Color : Allegro5.Color.ALLEGRO_COLOR;
   end record;

   procedure Render (P : in Pixel; E : in Entity);

   type Bitmap is new Display with record
      Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
   end record;

   overriding
   procedure Render (B : in Bitmap; E : in Entity);

   type Tinted_Scaled_Rotated_Bitmap is new Bitmap with record
      Tint : Color.ALLEGRO_COLOR := Color.al_map_rgba_f (r => 1.0,
                                                         g => 1.0,
                                                         b => 1.0,
                                                         a => 1.0);
      Scale : Vector_2D := (0.0, 0.0);
   end record;

   overriding
   procedure Render (B : in Tinted_Scaled_Rotated_Bitmap; E : in Entity);

   procedure Load_Bitmap (B : in out Bitmap'Class; Filename : in String);

   type Control is interface and Component;

   procedure Process (C : in Control; E : in out Entity) is abstract;

   type Acceleration_Control is new Control with record
      Accelerate, Decelerate : Keycodes.ALLEGRO_KEYCODE;
      Rate : Float;
   end record;

   overriding
   procedure Process (C : in Acceleration_Control; E : in out Entity);

   type Turn_Control is new Control with record
      Turn_Left, Turn_Right : Keycodes.ALLEGRO_KEYCODE;
      Rate : Float;
   end record;

   overriding
   procedure Process (C : in Turn_Control; E : in out Entity);

private

   type Entity is record
      Components : Component_Lists.List := Component_Lists.Empty_List;
   end record;

end Stardust_Engine.Entities;
