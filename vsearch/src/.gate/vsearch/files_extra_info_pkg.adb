with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Vsearch; use Callbacks_Vsearch;
with Vsearch_Intl; use Vsearch_Intl;
with Files_Extra_Info_Pkg.Callbacks; use Files_Extra_Info_Pkg.Callbacks;

package body Files_Extra_Info_Pkg is

procedure Gtk_New (Files_Extra_Info : out Files_Extra_Info_Access) is
begin
   Files_Extra_Info := new Files_Extra_Info_Record;
   Files_Extra_Info_Pkg.Initialize (Files_Extra_Info);
end Gtk_New;

procedure Initialize (Files_Extra_Info : access Files_Extra_Info_Record'Class) is
   pragma Suppress (All_Checks);
   Files_Combo_Items : String_List.Glist;
   Tooltips : Gtk_Tooltips;
   Directory_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Files_Extra_Info, Window_Toplevel);
   Set_Title (Files_Extra_Info, -"window1");
   Set_Policy (Files_Extra_Info, False, True, False);
   Set_Position (Files_Extra_Info, Win_Pos_None);
   Set_Modal (Files_Extra_Info, False);

   Gtk_New (Files_Extra_Info.Files_Frame, -"Files");
   Set_Shadow_Type (Files_Extra_Info.Files_Frame, Shadow_Etched_In);
   Add (Files_Extra_Info, Files_Extra_Info.Files_Frame);

   Gtk_New (Files_Extra_Info.Files_Table, 3, 3, False);
   Set_Border_Width (Files_Extra_Info.Files_Table, 5);
   Set_Row_Spacings (Files_Extra_Info.Files_Table, 5);
   Set_Col_Spacings (Files_Extra_Info.Files_Table, 5);
   Add (Files_Extra_Info.Files_Frame, Files_Extra_Info.Files_Table);

   Gtk_New (Files_Extra_Info.Files_Label, -("Files:"));
   Set_Alignment (Files_Extra_Info.Files_Label, 0.0, 0.5);
   Set_Padding (Files_Extra_Info.Files_Label, 0, 0);
   Set_Justify (Files_Extra_Info.Files_Label, Justify_Center);
   Set_Line_Wrap (Files_Extra_Info.Files_Label, False);
   Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Files_Label, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Files_Extra_Info.Directory_Label, -("Directory:"));
   Set_Alignment (Files_Extra_Info.Directory_Label, 0.0, 0.5);
   Set_Padding (Files_Extra_Info.Directory_Label, 0, 0);
   Set_Justify (Files_Extra_Info.Directory_Label, Justify_Center);
   Set_Line_Wrap (Files_Extra_Info.Directory_Label, False);
   Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Directory_Label, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Files_Extra_Info.Files_Combo);
   Set_Case_Sensitive (Files_Extra_Info.Files_Combo, False);
   Set_Use_Arrows (Files_Extra_Info.Files_Combo, True);
   Set_Use_Arrows_Always (Files_Extra_Info.Files_Combo, False);
   String_List.Append (Files_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Files_Extra_Info.Files_Combo, Files_Combo_Items);
   Free_String_List (Files_Combo_Items);
   Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Files_Combo, 1, 3, 0, 1,
     Fill, 0,
     0, 0);

   Files_Extra_Info.Files_Entry := Get_Entry (Files_Extra_Info.Files_Combo);
   Set_Editable (Files_Extra_Info.Files_Entry, True);
   Set_Max_Length (Files_Extra_Info.Files_Entry, 0);
   Set_Text (Files_Extra_Info.Files_Entry, -"");
   Set_Visibility (Files_Extra_Info.Files_Entry, True);
   Gtk_New (Tooltips);
   Set_Tip (Tooltips, Files_Extra_Info.Files_Entry, -"File(s) to scan");

   Gtk_New (Files_Extra_Info.Directory_Combo);
   Set_Case_Sensitive (Files_Extra_Info.Directory_Combo, False);
   Set_Use_Arrows (Files_Extra_Info.Directory_Combo, True);
   Set_Use_Arrows_Always (Files_Extra_Info.Directory_Combo, False);
   String_List.Append (Directory_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Files_Extra_Info.Directory_Combo, Directory_Combo_Items);
   Free_String_List (Directory_Combo_Items);
   Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Directory_Combo, 1, 3, 1, 2,
     Fill, 0,
     0, 0);

   Files_Extra_Info.Directory_Entry := Get_Entry (Files_Extra_Info.Directory_Combo);
   Set_Editable (Files_Extra_Info.Directory_Entry, True);
   Set_Max_Length (Files_Extra_Info.Directory_Entry, 0);
   Set_Text (Files_Extra_Info.Directory_Entry, -"");
   Set_Visibility (Files_Extra_Info.Directory_Entry, True);
   Set_Tip (Tooltips, Files_Extra_Info.Directory_Entry, -"Directory to scan");

   Gtk_New (Files_Extra_Info.Subdirs_Check, -"Recursive Search");
   Set_Active (Files_Extra_Info.Subdirs_Check, False);
   Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Subdirs_Check, 2, 3, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (Files_Extra_Info.Browse_Button, -"Browse...");
   Set_Relief (Files_Extra_Info.Browse_Button, Relief_Normal);
   Attach (Files_Extra_Info.Files_Table, Files_Extra_Info.Browse_Button, 1, 2, 2, 3,
     Fill, 0,
     0, 0);
   Set_Tip (Tooltips, Files_Extra_Info.Browse_Button, -"Select a directory");
   Widget_Callback.Object_Connect
     (Files_Extra_Info.Browse_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Browse_Button_Clicked'Access), Files_Extra_Info);

end Initialize;

end Files_Extra_Info_Pkg;
