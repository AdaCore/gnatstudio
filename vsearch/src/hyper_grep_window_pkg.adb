with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Hypergrep; use Callbacks_Hypergrep;
with Hyper_Grep_Window_Pkg.Callbacks; use Hyper_Grep_Window_Pkg.Callbacks;

with Gtk.Main; use Gtk.Main;

package body Hyper_Grep_Window_Pkg is

   ----------------------
   -- Files_Activation --
   ----------------------

   procedure Files_Activation
     (Hyper_Grep_Window : in out Hyper_Grep_Window_Access;
      Active            : Boolean := True)
   is
   begin
      Set_Sensitive (Hyper_Grep_Window.Directory_Combo, Active);
      Set_Sensitive (Hyper_Grep_Window.Browse_Button,   Active);
      Set_Sensitive (Hyper_Grep_Window.Subdirs_Check,   Active);
      Set_Sensitive (Hyper_Grep_Window.Files_Combo,     Active);
   end Files_Activation;

   ------------------
   -- Close_Window --
   ------------------

   procedure Close_Window
     (Hyper_Grep_Window : in out Hyper_Grep_Window_Access)
   is
   begin
      Destroy (Hyper_Grep_Window);
      Hyper_Grep_Window := null;

      Gtk.Main.Main_Quit;
   end Close_Window;

procedure Gtk_New (Hyper_Grep_Window : out Hyper_Grep_Window_Access) is
begin
   Hyper_Grep_Window := new Hyper_Grep_Window_Record;
   Hyper_Grep_Window_Pkg.Initialize (Hyper_Grep_Window);

   Files_Activation (Hyper_Grep_Window,
                     not Get_Active (Hyper_Grep_Window.Only_Project_Check));
end Gtk_New;

procedure Initialize (Hyper_Grep_Window : access Hyper_Grep_Window_Record'Class) is
   pragma Suppress (All_Checks);
   Pattern_Combo_Items : String_List.Glist;
   Directory_Combo_Items : String_List.Glist;
   Files_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Hyper_Grep_Window, Window_Toplevel);
   Set_Title (Hyper_Grep_Window, "Hyper grep");
   Set_Policy (Hyper_Grep_Window, False, True, False);
   Set_Position (Hyper_Grep_Window, Win_Pos_None);
   Set_Modal (Hyper_Grep_Window, False);
   Return_Callback.Connect
     (Hyper_Grep_Window, "delete_event", On_Hyper_Grep_Delete_Event'Access);

   Gtk_New_Vbox (Hyper_Grep_Window.Main_Table, False, 0);
   Add (Hyper_Grep_Window, Hyper_Grep_Window.Main_Table);

   Gtk_New (Hyper_Grep_Window.Search_Frame, "Search");
   Set_Border_Width (Hyper_Grep_Window.Search_Frame, 5);
   Set_Shadow_Type (Hyper_Grep_Window.Search_Frame, Shadow_Etched_In);
   Pack_Start (Hyper_Grep_Window.Main_Table, Hyper_Grep_Window.Search_Frame, False, False, 0);

   Gtk_New (Hyper_Grep_Window.Search_Table, 5, 3, False);
   Set_Border_Width (Hyper_Grep_Window.Search_Table, 5);
   Set_Row_Spacings (Hyper_Grep_Window.Search_Table, 0);
   Set_Col_Spacings (Hyper_Grep_Window.Search_Table, 0);
   Add (Hyper_Grep_Window.Search_Frame, Hyper_Grep_Window.Search_Table);

   Gtk_New (Hyper_Grep_Window.Statements_Check, "Statements");
   Set_Active (Hyper_Grep_Window.Statements_Check, True);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Statements_Check, 2, 3, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Strings_Check, "Strings");
   Set_Active (Hyper_Grep_Window.Strings_Check, True);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Strings_Check, 2, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Comments_Check, "Comments");
   Set_Active (Hyper_Grep_Window.Comments_Check, True);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Comments_Check, 2, 3, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Case_Check, "Match case");
   Set_Active (Hyper_Grep_Window.Case_Check, False);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Case_Check, 0, 2, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Whole_Word_Check, "Match whole word");
   Set_Active (Hyper_Grep_Window.Whole_Word_Check, False);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Whole_Word_Check, 0, 2, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Regexp_Check, "Regular expression");
   Set_Active (Hyper_Grep_Window.Regexp_Check, False);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Regexp_Check, 0, 2, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Search_For_Label, "Search for:");
   Set_Alignment (Hyper_Grep_Window.Search_For_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep_Window.Search_For_Label, 0, 0);
   Set_Justify (Hyper_Grep_Window.Search_For_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Window.Search_For_Label, False);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Search_For_Label, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Pattern_Combo);
   Set_Case_Sensitive (Hyper_Grep_Window.Pattern_Combo, False);
   Set_Use_Arrows (Hyper_Grep_Window.Pattern_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep_Window.Pattern_Combo, False);
   String_List.Append (Pattern_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep_Window.Pattern_Combo, Pattern_Combo_Items);
   Free_String_List (Pattern_Combo_Items);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Pattern_Combo, 1, 3, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep_Window.Pattern_Entry := Get_Entry (Hyper_Grep_Window.Pattern_Combo);
   Set_Editable (Hyper_Grep_Window.Pattern_Entry, True);
   Set_Max_Length (Hyper_Grep_Window.Pattern_Entry, 0);
   Set_Text (Hyper_Grep_Window.Pattern_Entry, "");
   Set_Visibility (Hyper_Grep_Window.Pattern_Entry, True);

   Gtk_New (Hyper_Grep_Window.Options_Label, "Options:");
   Set_Alignment (Hyper_Grep_Window.Options_Label, 0.0, 1.0);
   Set_Padding (Hyper_Grep_Window.Options_Label, 0, 0);
   Set_Justify (Hyper_Grep_Window.Options_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Window.Options_Label, False);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Options_Label, 0, 2, 1, 2,
     Fill, 0,
     0, 3);

   Gtk_New (Hyper_Grep_Window.Scan_In_Label, "Scan in:");
   Set_Alignment (Hyper_Grep_Window.Scan_In_Label, 0.0, 1.0);
   Set_Padding (Hyper_Grep_Window.Scan_In_Label, 0, 0);
   Set_Justify (Hyper_Grep_Window.Scan_In_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Window.Scan_In_Label, False);
   Attach (Hyper_Grep_Window.Search_Table, Hyper_Grep_Window.Scan_In_Label, 2, 3, 1, 2,
     Fill, 0,
     0, 3);

   Gtk_New (Hyper_Grep_Window.Files_Frame, "Files");
   Set_Border_Width (Hyper_Grep_Window.Files_Frame, 5);
   Set_Shadow_Type (Hyper_Grep_Window.Files_Frame, Shadow_Etched_In);
   Pack_Start (Hyper_Grep_Window.Main_Table, Hyper_Grep_Window.Files_Frame, False, False, 0);

   Gtk_New (Hyper_Grep_Window.Files_Table, 4, 3, False);
   Set_Border_Width (Hyper_Grep_Window.Files_Table, 5);
   Set_Row_Spacings (Hyper_Grep_Window.Files_Table, 5);
   Set_Col_Spacings (Hyper_Grep_Window.Files_Table, 5);
   Add (Hyper_Grep_Window.Files_Frame, Hyper_Grep_Window.Files_Table);

   Gtk_New (Hyper_Grep_Window.Browse_Button, "Browse...");
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Browse_Button, 2, 3, 2, 3,
     0, 0,
     0, 0);
   Button_Callback.Connect
     (Hyper_Grep_Window.Browse_Button, "clicked",
      Button_Callback.To_Marshaller (On_Browse_Button_Clicked'Access));

   Gtk_New (Hyper_Grep_Window.Files_Label, "Files:");
   Set_Alignment (Hyper_Grep_Window.Files_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep_Window.Files_Label, 0, 0);
   Set_Justify (Hyper_Grep_Window.Files_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Window.Files_Label, False);
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Files_Label, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Directory_Label, "Directory:");
   Set_Alignment (Hyper_Grep_Window.Directory_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep_Window.Directory_Label, 0, 0);
   Set_Justify (Hyper_Grep_Window.Directory_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Window.Directory_Label, False);
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Directory_Label, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Directory_Combo);
   Set_Case_Sensitive (Hyper_Grep_Window.Directory_Combo, False);
   Set_Use_Arrows (Hyper_Grep_Window.Directory_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep_Window.Directory_Combo, False);
   String_List.Append (Directory_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep_Window.Directory_Combo, Directory_Combo_Items);
   Free_String_List (Directory_Combo_Items);
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Directory_Combo, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep_Window.Directory_Entry := Get_Entry (Hyper_Grep_Window.Directory_Combo);
   Set_Editable (Hyper_Grep_Window.Directory_Entry, True);
   Set_Max_Length (Hyper_Grep_Window.Directory_Entry, 0);
   Set_Text (Hyper_Grep_Window.Directory_Entry, "");
   Set_Visibility (Hyper_Grep_Window.Directory_Entry, True);

   Gtk_New (Hyper_Grep_Window.Only_Project_Check, "Scan only project files");
   Set_Active (Hyper_Grep_Window.Only_Project_Check, True);
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Only_Project_Check, 0, 3, 0, 1,
     Fill, 0,
     0, 0);
   Check_Button_Callback.Connect
     (Hyper_Grep_Window.Only_Project_Check, "toggled",
      Check_Button_Callback.To_Marshaller (On_Only_Project_Check_Toggled'Access));

   Gtk_New (Hyper_Grep_Window.Subdirs_Check, "Include sub-directories");
   Set_Active (Hyper_Grep_Window.Subdirs_Check, False);
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Subdirs_Check, 1, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Window.Files_Combo);
   Set_Case_Sensitive (Hyper_Grep_Window.Files_Combo, False);
   Set_Use_Arrows (Hyper_Grep_Window.Files_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep_Window.Files_Combo, False);
   String_List.Append (Files_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep_Window.Files_Combo, Files_Combo_Items);
   Free_String_List (Files_Combo_Items);
   Attach (Hyper_Grep_Window.Files_Table, Hyper_Grep_Window.Files_Combo, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep_Window.Files_Entry := Get_Entry (Hyper_Grep_Window.Files_Combo);
   Set_Editable (Hyper_Grep_Window.Files_Entry, True);
   Set_Max_Length (Hyper_Grep_Window.Files_Entry, 0);
   Set_Text (Hyper_Grep_Window.Files_Entry, "");
   Set_Visibility (Hyper_Grep_Window.Files_Entry, True);

   Gtk_New (Hyper_Grep_Window.Hbuttonbox);
   Set_Spacing (Hyper_Grep_Window.Hbuttonbox, 30);
   Set_Layout (Hyper_Grep_Window.Hbuttonbox, Buttonbox_Spread);
   Set_Child_Size (Hyper_Grep_Window.Hbuttonbox, 85, 27);
   Set_Child_Ipadding (Hyper_Grep_Window.Hbuttonbox, 0, 0);
   Add (Hyper_Grep_Window.Main_Table, Hyper_Grep_Window.Hbuttonbox);

   Gtk_New (Hyper_Grep_Window.Start_Button, "Start");
   Set_Flags (Hyper_Grep_Window.Start_Button, Can_Default);
   Grab_Default (Hyper_Grep_Window.Start_Button);
   Button_Callback.Connect
     (Hyper_Grep_Window.Start_Button, "clicked",
      Button_Callback.To_Marshaller (On_Start_Button_Clicked'Access));
   Add (Hyper_Grep_Window.Hbuttonbox, Hyper_Grep_Window.Start_Button);

   Gtk_New (Hyper_Grep_Window.Stop_Button, "Stop");
   Set_Flags (Hyper_Grep_Window.Stop_Button, Can_Default);
   Button_Callback.Connect
     (Hyper_Grep_Window.Stop_Button, "clicked",
      Button_Callback.To_Marshaller (On_Stop_Button_Clicked'Access));
   Add (Hyper_Grep_Window.Hbuttonbox, Hyper_Grep_Window.Stop_Button);

   Gtk_New (Hyper_Grep_Window.Close_Button, "Close");
   Set_Flags (Hyper_Grep_Window.Close_Button, Can_Default);
   Button_Callback.Connect
     (Hyper_Grep_Window.Close_Button, "clicked",
      Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
   Add (Hyper_Grep_Window.Hbuttonbox, Hyper_Grep_Window.Close_Button);

end Initialize;

end Hyper_Grep_Window_Pkg;
