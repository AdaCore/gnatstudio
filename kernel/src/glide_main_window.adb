with Glide_Kernel; use Glide_Kernel;

package body Glide_Main_Window is

   procedure Gtk_New
     (Main_Window : out Glide_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      Main_Window := new Glide_Window_Record;
      Initialize (Main_Window, Key, Menu_Items);
   end Gtk_New;

   procedure Initialize
     (Main_Window : access Glide_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
      Gtk_New (Main_Window.Kernel, Main_Window);
   end Initialize;

end Glide_Main_Window;
