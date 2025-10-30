package body T is

   ----------------------
   -- Support_Language --
   ----------------------

   overriding
   function Support_Language
     (Self : access Outline_LSP_Provider; Lang : Language_Access)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Get_Language_Server (Lang) /= null;
   end Support_Language;

end T;
