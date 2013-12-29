with Ada.Tags;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Allegro5.Bitmap;

package Stardust_Engine.Entities is

   -- General

   type Vector_2D is record
      X, Y : Float;
   end record;

   type Vector_3D is record
      X, Y, Z : Float;
   end record;

   -- Component

   type Component is interface;

   type Null_Component_T is new Component with null record;

   Null_Component : constant Null_Component_T := Null_Component_T'(others => <>);

   -- Entity

   type Entity is private;

   procedure Add_Component (E : in out Entity; C : in Component'Class);

   function Get_Component (E : in Entity; CT : in Ada.Tags.Tag) return Component'Class;

   procedure Render (E : in Entity);

   -- Position

   type Position is interface and Component;

   type Position_2D is new Position with record
      Position : Vector_2D;
      Velocity : Vector_2D;
      Acceleration : Vector_2D;
   end record;

   type Position_3D is new Position with record
      Position : Vector_3D;
      Velocity : Vector_3D;
      Acceleration : Vector_3D;
   end record;

   procedure Move (E : in out Entity; dT : in Float);

   -- Attitude

   type Attitude is interface and Component;

   type Rotation is new Attitude with record
      R : Float;
   end record;

   -- Display

   type Display is interface and Component;

   procedure Render (D : in Display; E : in Entity) is abstract;

   type Display_Bitmap is new Display with private;

   procedure Load_Bitmap (DB : Display_Bitmap; Filename : String);

   procedure Render (DB : in Display_Bitmap; E : in Entity);

private

   package Component_Lists is new
      Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type => Component'Class);

   type Entity is record
      Components : Component_Lists.List := Component_Lists.Empty_List;
   end record;

   type Display_Bitmap is new Display with record
      Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
   end record;

end Stardust_Engine.Entities;
