with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Glide; use Callbacks_Glide;
with Builder_Pkg.Callbacks; use Builder_Pkg.Callbacks;

package body Builder_Pkg is

procedure Gtk_New (Builder : out Builder_Access) is
begin
   Builder := new Builder_Record;
   Builder_Pkg.Initialize (Builder);
end Gtk_New;

procedure Initialize (Builder : access Builder_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Builder, Window_Toplevel);
   Set_Title (Builder, "Builder");
   Set_Policy (Builder, False, True, False);
   Set_Position (Builder, Win_Pos_None);
   Set_Modal (Builder, False);
   Set_Default_Size (Builder, 500, 300);
   Return_Callback.Connect
     (Builder, "delete_event", On_Builder_Delete_Event'Access);

   Gtk_New_Vbox (Builder.Vbox1, False, 0);
   Add (Builder, Builder.Vbox1);

   Gtk_New (Builder.Frame1);
   Set_Shadow_Type (Builder.Frame1, Shadow_Etched_In);
   Pack_Start (Builder.Vbox1, Builder.Frame1, False, False, 0);

   Gtk_New_Hbox (Builder.Hbox1, False, 3);
   Set_Border_Width (Builder.Hbox1, 4);
   Add (Builder.Frame1, Builder.Hbox1);

   Gtk_New (Builder.Label1, "Build command");
   Set_Alignment (Builder.Label1, 0.5, 0.5);
   Set_Padding (Builder.Label1, 0, 0);
   Set_Justify (Builder.Label1, Justify_Center);
   Set_Line_Wrap (Builder.Label1, False);
   Pack_Start (Builder.Hbox1, Builder.Label1, False, False, 0);

   Gtk_New (Builder.Build_Entry);
   Set_Editable (Builder.Build_Entry, True);
   Set_Max_Length (Builder.Build_Entry, 0);
   Set_Text (Builder.Build_Entry, "");
   Set_Visibility (Builder.Build_Entry, True);
   Pack_Start (Builder.Hbox1, Builder.Build_Entry, True, True, 0);

   Gtk_New (Builder.Hbuttonbox1);
   Set_Spacing (Builder.Hbuttonbox1, 30);
   Set_Layout (Builder.Hbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Builder.Hbuttonbox1, 85, 27);
   Set_Child_Ipadding (Builder.Hbuttonbox1, 7, 0);
   Pack_Start (Builder.Vbox1, Builder.Hbuttonbox1, False, False, 0);

   Gtk_New (Builder.Build, "Build");
   Set_Flags (Builder.Build, Can_Default);
   Button_Callback.Connect
     (Builder.Build, "clicked",
      Button_Callback.To_Marshaller (On_Build_Clicked'Access));
   Add (Builder.Hbuttonbox1, Builder.Build);

   Gtk_New (Builder.Quit, "Quit");
   Set_Flags (Builder.Quit, Can_Default);
   Button_Callback.Connect
     (Builder.Quit, "clicked",
      Button_Callback.To_Marshaller (On_Quit_Clicked'Access));
   Add (Builder.Hbuttonbox1, Builder.Quit);

   Gtk_New (Builder.Scrolledwindow2);
   Set_Policy (Builder.Scrolledwindow2, Policy_Never, Policy_Always);
   Pack_Start (Builder.Vbox1, Builder.Scrolledwindow2, True, True, 0);

   Gtk_New (Builder.Source_Editor);
   Set_Editable (Builder.Source_Editor, False);
   Add (Builder.Scrolledwindow2, Builder.Source_Editor);

   Gtk_New (Builder.Scrolledwindow1);
   Set_Policy (Builder.Scrolledwindow1, Policy_Never, Policy_Always);
   Pack_Start (Builder.Vbox1, Builder.Scrolledwindow1, True, True, 0);

   Gtk_New (Builder.Output_Text);
   Set_Editable (Builder.Output_Text, False);
   Add (Builder.Scrolledwindow1, Builder.Output_Text);

   Gtk_New (Builder.Statusbar);
   Pack_Start (Builder.Vbox1, Builder.Statusbar, False, False, 0);

end Initialize;

end Builder_Pkg;
