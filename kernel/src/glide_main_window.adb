with Glide_Kernel; use Glide_Kernel;
with Gtk.Window; use Gtk.Window;

with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Arguments; use Gtk.Arguments;
with Gtkada.MDI;    use Gtkada.MDI;
with Glide_Page;    use Glide_Page;
with GVD.Process; use GVD.Process;


package body Glide_Main_Window is

   procedure Set_Focus (Win : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called when the widget that has the focus changes. This is used so that
   --  we can automatically change the focus in the MDI in such cases.

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
     (Win : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      pragma Warnings (Off, Win);
      Top  : Glide_Window := Glide_Window (Win);
      Widget : Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
      MDI    : constant MDI_Window :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Process_Mdi;
   begin
      if Widget /= null then
         Set_Focus_Child (MDI, Containing => Widget);
      end if;
   end Set_Focus;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Main_Window : out Glide_Window;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      Main_Window := new Glide_Window_Record;
      Glide_Main_Window.Initialize (Main_Window, Key, Menu_Items);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window : access Glide_Window_Record'Class;
      Key         : String;
      Menu_Items  : Gtk_Item_Factory_Entry_Array) is
   begin
      GVD.Main_Window.Initialize (Main_Window, Key, Menu_Items);
      Gtk_New (Main_Window.Kernel, Gtk_Window (Main_Window));

      Widget_Callback.Connect (Main_Window, "set_focus", Set_Focus'Access);
   end Initialize;

end Glide_Main_Window;
