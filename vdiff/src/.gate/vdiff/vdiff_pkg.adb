with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Vdiff; use Callbacks_Vdiff;
with Vdiff_Intl; use Vdiff_Intl;

package body Vdiff_Pkg is

procedure Gtk_New (Vdiff : out Vdiff_Access) is
begin
   Vdiff := new Vdiff_Record;
   Vdiff_Pkg.Initialize (Vdiff);
end Gtk_New;

procedure Initialize (Vdiff : access Vdiff_Record'Class) is
   pragma Suppress (All_Checks);
   Search_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Vdiff, Window_Toplevel);
   Set_Title (Vdiff, -"Visual Comparison");
   Set_Policy (Vdiff, False, True, False);
   Set_Position (Vdiff, Win_Pos_None);
   Set_Modal (Vdiff, False);

   Gtk_New_Vbox (Vdiff.Main_Box, False, 5);
   Add (Vdiff, Vdiff.Main_Box);

   Gtk_New (Vdiff.Toolbar, Orientation_Horizontal, Toolbar_Icons);
   Set_Border_Width (Vdiff.Toolbar, 4);
   Set_Space_Size (Vdiff.Toolbar, 5);
   Set_Space_Style (Vdiff.Toolbar, Toolbar_Space_Empty);
   Set_Tooltips (Vdiff.Toolbar, True);
   Set_Button_Relief (Vdiff.Toolbar, Relief_Normal);
   Vdiff.Button1 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button2 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button3 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button4 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button5 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button6 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button7 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Vdiff.Button8 := Append_Element
     (Toolbar => Vdiff.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"button1");
   Pack_Start (Vdiff.Main_Box, Vdiff.Toolbar, False, False, 0);

   Gtk_New (Vdiff.Search_Combo);
   Set_Case_Sensitive (Vdiff.Search_Combo, False);
   Set_Use_Arrows (Vdiff.Search_Combo, True);
   Set_Use_Arrows_Always (Vdiff.Search_Combo, False);
   String_List.Append (Search_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Vdiff.Search_Combo, Search_Combo_Items);
   Free_String_List (Search_Combo_Items);
   Append_Widget (Vdiff.Toolbar, Vdiff.Search_Combo);

   Vdiff.Combo_Entry1 := Get_Entry (Vdiff.Search_Combo);
   Set_Editable (Vdiff.Combo_Entry1, True);
   Set_Max_Length (Vdiff.Combo_Entry1, 0);
   Set_Text (Vdiff.Combo_Entry1, -"");
   Set_Visibility (Vdiff.Combo_Entry1, True);

   Gtk_New (Vdiff.Main_Frame);
   Set_Shadow_Type (Vdiff.Main_Frame, Shadow_Etched_In);
   Pack_Start (Vdiff.Main_Box, Vdiff.Main_Frame, True, True, 0);

   Gtk_New_Hbox (Vdiff.Hbox2, False, 0);
   Add (Vdiff.Main_Frame, Vdiff.Hbox2);

   Gtk_New_Vbox (Vdiff.Vbox2, False, 0);
   Pack_Start (Vdiff.Hbox2, Vdiff.Vbox2, True, True, 0);

   Gtk_New_Hbox (Vdiff.Hbox4, False, 0);
   Pack_Start (Vdiff.Vbox2, Vdiff.Hbox4, False, False, 0);

   Gtk_New (Vdiff.Label5, -("File1:"));
   Set_Alignment (Vdiff.Label5, 0.5, 0.5);
   Set_Padding (Vdiff.Label5, 0, 0);
   Set_Justify (Vdiff.Label5, Justify_Center);
   Set_Line_Wrap (Vdiff.Label5, False);
   Pack_Start (Vdiff.Hbox4, Vdiff.Label5, False, False, 5);

   Gtk_New (Vdiff.Frame3);
   Set_Shadow_Type (Vdiff.Frame3, Shadow_In);
   Pack_Start (Vdiff.Hbox4, Vdiff.Frame3, True, True, 0);

   Gtk_New (Vdiff.File_Label1, -("File"));
   Set_Alignment (Vdiff.File_Label1, 7.45058e-09, 0.5);
   Set_Padding (Vdiff.File_Label1, 0, 0);
   Set_Justify (Vdiff.File_Label1, Justify_Center);
   Set_Line_Wrap (Vdiff.File_Label1, False);
   Add (Vdiff.Frame3, Vdiff.File_Label1);

   Gtk_New_Hbox (Vdiff.File1_Box, False, 5);
   Set_Border_Width (Vdiff.File1_Box, 5);
   Pack_Start (Vdiff.Vbox2, Vdiff.File1_Box, True, True, 0);

   Gtk_New (Vdiff.Frame5);
   Set_Shadow_Type (Vdiff.Frame5, Shadow_In);
   Pack_Start (Vdiff.File1_Box, Vdiff.Frame5, False, True, 0);

   Gtk_New (Vdiff.Drawingarea1);
   Add (Vdiff.Frame5, Vdiff.Drawingarea1);

   Gtk_New (Vdiff.Scrolledwindow1);
   Set_Policy (Vdiff.Scrolledwindow1, Policy_Always, Policy_Always);
   Pack_Start (Vdiff.File1_Box, Vdiff.Scrolledwindow1, True, True, 0);

   Gtk_New (Vdiff.Clist1, 2);
   Set_Selection_Mode (Vdiff.Clist1, Selection_Single);
   Set_Shadow_Type (Vdiff.Clist1, Shadow_In);
   Set_Show_Titles (Vdiff.Clist1, False);
   Set_Column_Width (Vdiff.Clist1, 0, 32);
   Set_Column_Width (Vdiff.Clist1, 1, 80);
   Add (Vdiff.Scrolledwindow1, Vdiff.Clist1);

   Gtk_New (Vdiff.Label8);
   Set_Alignment (Vdiff.Label8, 0.5, 0.5);
   Set_Padding (Vdiff.Label8, 0, 0);
   Set_Justify (Vdiff.Label8, Justify_Center);
   Set_Line_Wrap (Vdiff.Label8, False);
   Set_Column_Widget (Vdiff.Clist1, 0, Vdiff.Label8);

   Gtk_New (Vdiff.Label9);
   Set_Alignment (Vdiff.Label9, 0.5, 0.5);
   Set_Padding (Vdiff.Label9, 0, 0);
   Set_Justify (Vdiff.Label9, Justify_Center);
   Set_Line_Wrap (Vdiff.Label9, False);
   Set_Column_Widget (Vdiff.Clist1, 1, Vdiff.Label9);

   Gtk_New_Vseparator (Vdiff.Vseparator1);
   Pack_Start (Vdiff.Hbox2, Vdiff.Vseparator1, False, True, 5);

   Gtk_New_Vbox (Vdiff.Vbox3, False, 0);
   Pack_Start (Vdiff.Hbox2, Vdiff.Vbox3, True, True, 0);

   Gtk_New_Hbox (Vdiff.Hbox5, False, 0);
   Pack_Start (Vdiff.Vbox3, Vdiff.Hbox5, False, False, 0);

   Gtk_New (Vdiff.Label7, -("File2:"));
   Set_Alignment (Vdiff.Label7, 0.5, 0.5);
   Set_Padding (Vdiff.Label7, 0, 0);
   Set_Justify (Vdiff.Label7, Justify_Center);
   Set_Line_Wrap (Vdiff.Label7, False);
   Pack_Start (Vdiff.Hbox5, Vdiff.Label7, False, False, 5);

   Gtk_New (Vdiff.Frame4);
   Set_Shadow_Type (Vdiff.Frame4, Shadow_In);
   Pack_Start (Vdiff.Hbox5, Vdiff.Frame4, True, True, 0);

   Gtk_New (Vdiff.File_Label2, -("File"));
   Set_Alignment (Vdiff.File_Label2, 7.45058e-09, 0.5);
   Set_Padding (Vdiff.File_Label2, 0, 0);
   Set_Justify (Vdiff.File_Label2, Justify_Center);
   Set_Line_Wrap (Vdiff.File_Label2, False);
   Add (Vdiff.Frame4, Vdiff.File_Label2);

   Gtk_New_Hbox (Vdiff.File2_Box, False, 5);
   Set_Border_Width (Vdiff.File2_Box, 5);
   Pack_Start (Vdiff.Vbox3, Vdiff.File2_Box, True, True, 0);

   Gtk_New (Vdiff.Frame6);
   Set_Shadow_Type (Vdiff.Frame6, Shadow_In);
   Pack_Start (Vdiff.File2_Box, Vdiff.Frame6, False, True, 0);

   Gtk_New (Vdiff.Drawingarea2);
   Add (Vdiff.Frame6, Vdiff.Drawingarea2);

   Gtk_New (Vdiff.Scrolledwindow2);
   Set_Policy (Vdiff.Scrolledwindow2, Policy_Always, Policy_Always);
   Pack_Start (Vdiff.File2_Box, Vdiff.Scrolledwindow2, True, True, 0);

   Gtk_New (Vdiff.Clist2, 2);
   Set_Selection_Mode (Vdiff.Clist2, Selection_Single);
   Set_Shadow_Type (Vdiff.Clist2, Shadow_In);
   Set_Show_Titles (Vdiff.Clist2, False);
   Set_Column_Width (Vdiff.Clist2, 0, 36);
   Set_Column_Width (Vdiff.Clist2, 1, 80);
   Add (Vdiff.Scrolledwindow2, Vdiff.Clist2);

   Gtk_New (Vdiff.Label10);
   Set_Alignment (Vdiff.Label10, 0.5, 0.5);
   Set_Padding (Vdiff.Label10, 0, 0);
   Set_Justify (Vdiff.Label10, Justify_Center);
   Set_Line_Wrap (Vdiff.Label10, False);
   Set_Column_Widget (Vdiff.Clist2, 0, Vdiff.Label10);

   Gtk_New (Vdiff.Label11);
   Set_Alignment (Vdiff.Label11, 0.5, 0.5);
   Set_Padding (Vdiff.Label11, 0, 0);
   Set_Justify (Vdiff.Label11, Justify_Center);
   Set_Line_Wrap (Vdiff.Label11, False);
   Set_Column_Widget (Vdiff.Clist2, 1, Vdiff.Label11);

   Gtk_New (Vdiff.Statusbar);
   Pack_Start (Vdiff.Main_Box, Vdiff.Statusbar, False, False, 0);

end Initialize;

end Vdiff_Pkg;
