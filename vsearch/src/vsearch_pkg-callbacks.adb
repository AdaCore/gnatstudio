with Gtkada.File_Selection; use Gtkada.File_Selection;
with Glide_Intl; use Glide_Intl;
with GNAT.OS_Lib;

package body Vsearch_Pkg.Callbacks is

   --  use Gtk.Arguments;

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Object);
      S       : constant String :=
        File_Selection_Dialog
         (-"Select a directory",
          "." & GNAT.OS_Lib.Directory_Separator,
          Dir_Only   => True,
          Must_Exist => True);

   begin
      if S /= "" then
         Set_Text (Vsearch.Directory_Entry, S);
      end if;
   end On_Browse_Button_Clicked;

end Vsearch_Pkg.Callbacks;
