with Gtkada.MDI; use Gtkada.MDI;
with Gtk.Enums;  use Gtk.Enums;

package body Glide_Page is

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class) is
   begin
      Page := new Glide_Page_Record;
      Initialize (Page, Window);
   end Gtk_New;

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class)
   is
      Child : MDI_Child;
   begin
      GVD.Process.Initialize (Page, Window);

      Gtk_New (Page.Console_Sw);
      Set_Policy (Page.Console_Sw, Policy_Never, Policy_Always);
      Child := Put (Page.Process_Mdi, Page.Console_Sw);
      Set_Title (Child, "Glide Console");
      Dock_Child (Child, Side => Bottom);

      Gtk_New (Page.Console);
      Set_Editable (Page.Console, False);
      Add (Page.Console_Sw, Page.Console);
   end Initialize;

end Glide_Page;
