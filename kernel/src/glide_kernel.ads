--  This package is the root of the glide's kernel API.

package Glide_Kernel is
   pragma Pure;

   type Kernel_Handle is private;
   --  A kernel handle used to share information throughout Glide.
   --  ??? Consider whether using a tagged type would be useful.

private

   type Kernel_Handle is null record;

end Glide_Kernel;
