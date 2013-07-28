with Allegro5.Color;

package Stardust_Engine.Particles is
   type Particle is new Object_2 and Drawable with
      record
         Age : Integer; -- in fps
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
      Parts : Object_Lists.List := Object_Lists.Empty_List;
   end Particle_System;
end Stardust_Engine.Particles;
