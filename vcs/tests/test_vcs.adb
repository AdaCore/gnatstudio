with Gtk; use Gtk;
with Gtk.Main;

with Gdk.Pixmap;  use Gdk.Pixmap;
with Gdk.Bitmap;  use Gdk.Bitmap;
with Gdk.Color;   use Gdk.Color;
with VCS_Filters; use VCS_Filters;

with VCS.CVS;     use VCS.CVS;

with Gtkada.File_Selector; use Gtkada.File_Selector;

with Pixmaps_IDE; use Pixmaps_IDE;

procedure Test_VCS is

   File_Selector_Window : File_Selector_Window_Access;
   Filter : Filter_VCS_Access := new Filter_VCS;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (File_Selector_Window, "", "Test", "Test");

--    Create_From_Xpm_D
--      (Filter.Not_Registered_Pixmap,
--       Window => null,
--       Colormap => Get_System,
--       Mask => Filter.Not_Registered_Bitmap,
--       Transparent => Null_Color,
--       Data => box_xpm);

   Filter.VCS_Id := new CVS_Record;

   Filter.Query_Unknown := False;

   Filter.Unknown_Bitmap := Null_Bitmap;
   Filter.Unknown_Pixmap := Null_Pixmap;

   Create_From_Xpm_D
     (Filter.Not_Registered_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter.Not_Registered_Bitmap,
      Transparent => Null_Color,
      Data => stock_close_xpm);

   Create_From_Xpm_D
     (Filter.Up_To_Date_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter.Up_To_Date_Bitmap,
      Transparent => Null_Color,
      Data => stock_button_apply_xpm);

   Create_From_Xpm_D
     (Filter.Modified_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter.Modified_Bitmap,
      Transparent => Null_Color,
      Data => stock_preferences_xpm);

   Create_From_Xpm_D
     (Filter.Needs_Merge_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter.Needs_Merge_Bitmap,
      Transparent => Null_Color,
      Data => stop_xpm);

   Create_From_Xpm_D
     (Filter.Needs_Update_Pixmap,
      Window => null,
      Colormap => Get_System,
      Mask => Filter.Needs_Update_Bitmap,
      Transparent => Null_Color,
      Data => stock_button_cancel_xpm);

   Register_Filter (File_Selector_Window, Filter);

   Show_All (File_Selector_Window);
   Gtk.Main.Main;

end Test_VCS;
