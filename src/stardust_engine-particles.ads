

package Stardust_Engine.Particles is

   type Particle is new Entities.Entity with record
      Color : Color.ALLEGRO_COLOR;
      Time_To_Live : Duration;
   end Particle;

   function New_Pixel (Color : Color.ALLEGRO_COLOR; Time_To_Life : Duration);

end Stardust_Engine.Particles;
