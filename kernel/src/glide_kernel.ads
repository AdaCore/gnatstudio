--  This package is the root of the glide's kernel API.

with Prj;

package Glide_Kernel is

   type Kernel_Handle is private;
   --  A kernel handle used to share information throughout Glide.
   --  ??? Consider whether using a tagged type would be useful.

private

   type Kernel_Handle is record
      Project : Prj.Project_Id;
      --  The project currently loaded.
   end record;

end Glide_Kernel;
