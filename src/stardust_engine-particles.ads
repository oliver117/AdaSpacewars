with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Allegro5.Color;

package Stardust_Engine.Particles is
   type Particle is new Object_2 and Drawable with
      record
         TTL : Integer; -- in fps
         Color : Allegro5.Color.ALLEGRO_COLOR;
      end record;

   -- The game has to supply a custom draw and move procedure per
   -- particle system. Therefore the inherited abstract procedure
   -- is overridden with a null procedure;
   overriding
   procedure Draw (P : Particle) is null;

   generic
      type Particle_T (<>) is new Particle with private;
      with procedure Draw_Particle (P : Particle_T);
      with procedure Move_Particle (P : in out Particle_T; dT : Duration) is Move;
   package Particle_System is
      package Particle_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Particle_T);

      Particles : Particle_Lists.List := Particle_Lists.Empty_List;
      type System_Handle is new Drawable and Movable with private;

      function Get_Handle return System_Handle;

      procedure Draw (SH : System_Handle);
      procedure Move (SH : in out System_Handle; dT : Duration);

   private
      type System_Handle is new Drawable and Movable with null record;
      Handle : System_Handle;
   end Particle_System;

end Stardust_Engine.Particles;
