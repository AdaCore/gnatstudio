with Gtk.Main; use Gtk.Main;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with GNAT.IO; use GNAT.IO;

package body Open_Program_Pkg.Callbacks is

   -----------------------------
   -- On_Radio_Button_Toggled --
   -----------------------------

   procedure On_Radio_Button_Toggled
     (Object : access Gtk_Radio_Button_Record'Class)
   is
   begin
      null;
   end On_Radio_Button_Toggled;

   ----------------------------
   -- On_Open_Button_Clicked --
   ----------------------------

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      S : String := File_Selection_Dialog;
   begin
      if S /= "" then
         Set_Text
           (Get_Entry
             (Open_Program_Access (Get_Toplevel (Object)).Program_Combo), S);
      end if;
   end On_Open_Button_Clicked;

   ------------------------
   -- On_Ok_Open_Clicked --
   ------------------------

   procedure On_Ok_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Open_Program_Access (Get_Toplevel (Object)).Valid := True;
      Main_Quit;
   end On_Ok_Open_Clicked;

   ----------------------------
   -- On_Cancel_Open_Clicked --
   ----------------------------

   procedure On_Cancel_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Open_Program_Access (Get_Toplevel (Object)).Valid := False;
      Main_Quit;
   end On_Cancel_Open_Clicked;

   --------------------------
   -- On_Help_Open_Clicked --
   --------------------------

   procedure On_Help_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Open_Clicked;

end Open_Program_Pkg.Callbacks;
