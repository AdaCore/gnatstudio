with GVD.Main_Window;
with Glide_Kernel;
with Gtk.Item_Factory; use Gtk.Item_Factory;

package Glide_Main_Window is

   type Glide_Window_Record is new GVD.Main_Window.GVD_Main_Window_Record with
   record
      Kernel      : Glide_Kernel.Kernel_Handle;
      Interrupted : Boolean := False;
   end record;
   type Glide_Window is access all Glide_Window_Record'Class;

   procedure Gtk_New
     (Main_Window : out Glide_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array);
   --  Create a new main window.
   --  Key is a unique string identifying main_window.
   --  Menu_Items is used to create the menu bar.

   procedure Initialize
     (Main_Window : access Glide_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array);
   --  Internal initialization function.

end Glide_Main_Window;
