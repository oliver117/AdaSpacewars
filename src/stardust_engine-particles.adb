with Allegro5.Color;
with Allegro5.Display;

generic
   with procedure Draw (P : Particle
package Stardust_Engine.Particles is
   type Particle is new Object_2 with
      record
         Age : Integer; -- in fps
         Color : Allegro5.Color.ALLEGRO_COLOR;
      end record;

   procedure Move (P : in out Particle; dT : Duration) is
   begin
      Object_2 (P).Move;
      P.Age := P.Age += 1;
   end Move;
end Stardust_Engine.Particles;
