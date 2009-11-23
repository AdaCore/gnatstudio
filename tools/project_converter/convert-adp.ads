
--  This package converts from .adp projects to .gpr projects

with GNAT.OS_Lib;

package Convert.Adp is

   procedure Convert_From_Adp_To_Gpr
     (Adp_Filename : String;
      Spec_Extension, Body_Extension : GNAT.OS_Lib.String_Access);
   --  Convert project is sent to standard output

end Convert.Adp;
