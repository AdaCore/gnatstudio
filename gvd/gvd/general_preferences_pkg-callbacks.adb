with Gtk.Widget; use Gtk.Widget;

package body Odd_Preferences_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------------------
   -- On_Odd_Preferences_Delete_Event --
   -------------------------------------

   procedure On_Odd_Preferences_Delete_Event
     (Object : access Gtk_Window_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Hide (Object);
      --  return False;
   end On_Odd_Preferences_Delete_Event;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Get_Toplevel (Object));
   end On_Ok_Button_Clicked;

   -----------------------------
   -- On_Reset_Button_Clicked --
   -----------------------------

   procedure On_Reset_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Reset_Button_Clicked;

   ----------------------------
   -- On_Help_Button_Clicked --
   ----------------------------

   procedure On_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Button_Clicked;

end Odd_Preferences_Pkg.Callbacks;
