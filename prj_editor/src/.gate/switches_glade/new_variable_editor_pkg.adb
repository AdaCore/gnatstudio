with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Switches_Glade_Intl; use Switches_Glade_Intl;

package body New_Variable_Editor_Pkg is

procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access) is
begin
   New_Variable_Editor := new New_Variable_Editor_Record;
   New_Variable_Editor_Pkg.Initialize (New_Variable_Editor);
end Gtk_New;

procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Variable_Name_Items : String_List.Glist;

begin
   Gtk.Dialog.Initialize (New_Variable_Editor);
   Set_Title (New_Variable_Editor, -"Creating a variable");
   Set_Policy (New_Variable_Editor, True, True, False);
   Set_Position (New_Variable_Editor, Win_Pos_None);
   Set_Modal (New_Variable_Editor, False);
   Set_Default_Size (New_Variable_Editor, 600, 400);

   New_Variable_Editor.Dialog_Vbox1 := Get_Vbox (New_Variable_Editor);
   Set_Homogeneous (New_Variable_Editor.Dialog_Vbox1, False);
   Set_Spacing (New_Variable_Editor.Dialog_Vbox1, 0);

   Gtk_New (New_Variable_Editor.Table1, 2, 2, False);
   Set_Border_Width (New_Variable_Editor.Table1, 10);
   Set_Row_Spacings (New_Variable_Editor.Table1, 5);
   Set_Col_Spacings (New_Variable_Editor.Table1, 0);
   Pack_Start (New_Variable_Editor.Dialog_Vbox1, New_Variable_Editor.Table1, True, True, 0);

   Gtk_New (New_Variable_Editor.Label58, -("Name:"));
   Set_Alignment (New_Variable_Editor.Label58, 0.0, 0.5);
   Set_Padding (New_Variable_Editor.Label58, 10, 0);
   Set_Justify (New_Variable_Editor.Label58, Justify_Left);
   Set_Line_Wrap (New_Variable_Editor.Label58, False);
   Attach (New_Variable_Editor.Table1, New_Variable_Editor.Label58, 0, 1, 0, 1,
     Fill, Fill,
     0, 0);

   Gtk_New (New_Variable_Editor.Variable_Name);
   Set_Case_Sensitive (New_Variable_Editor.Variable_Name, True);
   Set_Use_Arrows (New_Variable_Editor.Variable_Name, True);
   Set_Use_Arrows_Always (New_Variable_Editor.Variable_Name, False);
   String_List.Append (Variable_Name_Items, -"");
   Combo.Set_Popdown_Strings (New_Variable_Editor.Variable_Name, Variable_Name_Items);
   Free_String_List (Variable_Name_Items);
   Attach (New_Variable_Editor.Table1, New_Variable_Editor.Variable_Name, 1, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);

   New_Variable_Editor.Combo_Entry9 := Get_Entry (New_Variable_Editor.Variable_Name);
   Set_Editable (New_Variable_Editor.Combo_Entry9, True);
   Set_Max_Length (New_Variable_Editor.Combo_Entry9, 0);
   Set_Text (New_Variable_Editor.Combo_Entry9, -"");
   Set_Visibility (New_Variable_Editor.Combo_Entry9, True);

   Gtk_New (New_Variable_Editor.Label60, -("Possible values:"));
   Set_Alignment (New_Variable_Editor.Label60, 7.45058e-09, 7.45058e-09);
   Set_Padding (New_Variable_Editor.Label60, 10, 0);
   Set_Justify (New_Variable_Editor.Label60, Justify_Center);
   Set_Line_Wrap (New_Variable_Editor.Label60, False);
   Attach (New_Variable_Editor.Table1, New_Variable_Editor.Label60, 0, 1, 1, 2,
     Fill, Fill,
     0, 0);

   Gtk_New (New_Variable_Editor.Scrolledwindow2);
   Set_Policy (New_Variable_Editor.Scrolledwindow2, Policy_Automatic, Policy_Automatic);
   Attach (New_Variable_Editor.Table1, New_Variable_Editor.Scrolledwindow2, 1, 2, 1, 2,
     Fill, Expand or Shrink or Fill,
     0, 0);

   Gtk_New (New_Variable_Editor.Viewport1);
   Set_Shadow_Type (New_Variable_Editor.Viewport1, Shadow_In);
   Add (New_Variable_Editor.Scrolledwindow2, New_Variable_Editor.Viewport1);

   Gtk_New_Vbox (New_Variable_Editor.Vbox54, False, 0);
   Add (New_Variable_Editor.Viewport1, New_Variable_Editor.Vbox54);

   Gtk_New (New_Variable_Editor.Values_List, 1);
   Set_Selection_Mode (New_Variable_Editor.Values_List, Selection_Single);
   Set_Shadow_Type (New_Variable_Editor.Values_List, Shadow_In);
   Set_Show_Titles (New_Variable_Editor.Values_List, False);
   Set_Column_Width (New_Variable_Editor.Values_List, 0, 80);
   Pack_Start (New_Variable_Editor.Vbox54, New_Variable_Editor.Values_List, True, True, 0);

   Gtk_New (New_Variable_Editor.Label61, -("label61"));
   Set_Alignment (New_Variable_Editor.Label61, 0.5, 0.5);
   Set_Padding (New_Variable_Editor.Label61, 0, 0);
   Set_Justify (New_Variable_Editor.Label61, Justify_Center);
   Set_Line_Wrap (New_Variable_Editor.Label61, False);
   Set_Column_Widget (New_Variable_Editor.Values_List, 0, New_Variable_Editor.Label61);

   Gtk_New (New_Variable_Editor.Hbuttonbox4);
   Set_Spacing (New_Variable_Editor.Hbuttonbox4, 30);
   Set_Layout (New_Variable_Editor.Hbuttonbox4, Buttonbox_End);
   Set_Child_Size (New_Variable_Editor.Hbuttonbox4, 85, 27);
   Set_Child_Ipadding (New_Variable_Editor.Hbuttonbox4, 7, 0);
   Pack_Start (New_Variable_Editor.Vbox54, New_Variable_Editor.Hbuttonbox4, False, False, 0);

   Gtk_New (New_Variable_Editor.Delete_Variable, -"Delete");
   Set_Flags (New_Variable_Editor.Delete_Variable, Can_Default);
   Add (New_Variable_Editor.Hbuttonbox4, New_Variable_Editor.Delete_Variable);

   Gtk_New (New_Variable_Editor.New_Variable, -"New");
   Set_Flags (New_Variable_Editor.New_Variable, Can_Default);
   Add (New_Variable_Editor.Hbuttonbox4, New_Variable_Editor.New_Variable);

   Gtk_New (New_Variable_Editor.Rename_Variable, -"Rename");
   Set_Flags (New_Variable_Editor.Rename_Variable, Can_Default);
   Add (New_Variable_Editor.Hbuttonbox4, New_Variable_Editor.Rename_Variable);

   New_Variable_Editor.Dialog_Action_Area1 := Get_Action_Area (New_Variable_Editor);
   Set_Border_Width (New_Variable_Editor.Dialog_Action_Area1, 10);
   Set_Homogeneous (New_Variable_Editor.Dialog_Action_Area1, True);
   Set_Spacing (New_Variable_Editor.Dialog_Action_Area1, 5);

end Initialize;

end New_Variable_Editor_Pkg;
