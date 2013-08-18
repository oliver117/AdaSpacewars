
package body Stardust_Engine.Particles is

   package body Particle_System is
      function Get_Handle return System_Handle is
      begin
         return Handle;
      end Get_Handle;

      procedure Draw (SH : in System_Handle) is
         procedure Each (Cur : Particle_Lists.Cursor) is
            P : constant Particle_T := Particle_Lists.Element (Cur);
         begin
           Draw_Particle (P);
         end Each;
      begin
         Particles.Iterate (Each'Access);
      end Draw;

      procedure Move (SH : in out System_Handle; dT : Duration) is
         procedure Each (Cur : Particle_Lists.Cursor) is
            P : Particle_T := Particle_Lists.Element (Cur);
         begin
            Move_Particle (P, dT);
            P.TTL := P.TTL - 1;

            Particles.Replace_Element (Cur, P);
         end Each;
      begin
         Particles.Iterate (Each'Access);
      end Move;

   end Particle_System;

end Stardust_Engine.Particles;
