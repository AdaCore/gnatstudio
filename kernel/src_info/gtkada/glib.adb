package body Glib is

   function Boxed_Type_Register_Static
     (Name : String;
      Copy : Boxed_Copy;
      Free : Boxed_Free) return GType
   is
      pragma Unreferenced (Name, Copy, Free);
   begin
      return GType_None;
   end Boxed_Type_Register_Static;
end Glib;
