with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Radical; use Callbacks_Radical;
with Radical_Intl; use Radical_Intl;
with Inspector_Pkg.Callbacks; use Inspector_Pkg.Callbacks;

package body Inspector_Pkg is

procedure Gtk_New (Inspector : out Inspector_Access) is
begin
   Inspector := new Inspector_Record;
   Inspector_Pkg.Initialize (Inspector);
end Gtk_New;

procedure Initialize (Inspector : access Inspector_Record'Class) is
   pragma Suppress (All_Checks);
   Combo1_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Inspector, Window_Dialog);
   Return_Callback.Connect
     (Inspector, "delete_event", On_Properties_Delete_Event'Access);
   Set_Title (Inspector, -"Widget Inspector");
   Set_Policy (Inspector, False, True, False);
   Set_Position (Inspector, Win_Pos_None);
   Set_Modal (Inspector, False);
   Set_Default_Size (Inspector, 250, 400);

   Gtk_New_Vbox (Inspector.Vbox2, False, 5);
   Add (Inspector, Inspector.Vbox2);
   Set_Border_Width (Inspector.Vbox2, 3);

   Gtk_New (Inspector.Combo1);
   Pack_Start (Inspector.Vbox2, Inspector.Combo1, False, False, 0);
   Set_Case_Sensitive (Inspector.Combo1, False);
   Set_Use_Arrows (Inspector.Combo1, True);
   Set_Use_Arrows_Always (Inspector.Combo1, False);
   String_List.Append (Combo1_Items, -"");
   Combo.Set_Popdown_Strings (Inspector.Combo1, Combo1_Items);
   Free_String_List (Combo1_Items);

   Inspector.Combo_Entry1 := Get_Entry (Inspector.Combo1);
   Set_Editable (Inspector.Combo_Entry1, True);
   Set_Max_Length (Inspector.Combo_Entry1, 0);
   Set_Text (Inspector.Combo_Entry1, -"");
   Set_Visibility (Inspector.Combo_Entry1, True);

   Gtk_New (Inspector.Notebook4);
   Pack_Start (Inspector.Vbox2, Inspector.Notebook4, True, True, 0);
   Set_Scrollable (Inspector.Notebook4, False);
   Set_Show_Border (Inspector.Notebook4, True);
   Set_Show_Tabs (Inspector.Notebook4, True);
   Set_Tab_Hborder (Inspector.Notebook4, 2);
   Set_Tab_Vborder (Inspector.Notebook4, 2);
   Set_Tab_Pos (Inspector.Notebook4, Pos_Top);

   Gtk_New (Inspector.Scrolledwindow4);
   Add (Inspector.Notebook4, Inspector.Scrolledwindow4);
   Set_Policy (Inspector.Scrolledwindow4, Policy_Automatic, Policy_Automatic);

   Gtk_New (Inspector.Clist1, 2);
   Add (Inspector.Scrolledwindow4, Inspector.Clist1);
   Set_Selection_Mode (Inspector.Clist1, Selection_Single);
   Set_Shadow_Type (Inspector.Clist1, Shadow_In);
   Set_Show_Titles (Inspector.Clist1, False);
   Set_Column_Width (Inspector.Clist1, 0, 80);
   Set_Column_Width (Inspector.Clist1, 1, 80);

   Gtk_New (Inspector.Label23, -("label3"));
   Set_Alignment (Inspector.Label23, 0.5, 0.5);
   Set_Padding (Inspector.Label23, 0, 0);
   Set_Justify (Inspector.Label23, Justify_Center);
   Set_Line_Wrap (Inspector.Label23, False);
   Set_Column_Widget (Inspector.Clist1, 0, Inspector.Label23);

   Gtk_New (Inspector.Label24, -("label4"));
   Set_Alignment (Inspector.Label24, 0.5, 0.5);
   Set_Padding (Inspector.Label24, 0, 0);
   Set_Justify (Inspector.Label24, Justify_Center);
   Set_Line_Wrap (Inspector.Label24, False);
   Set_Column_Widget (Inspector.Clist1, 1, Inspector.Label24);

   Gtk_New (Inspector.Label25, -("Properties"));
   Set_Alignment (Inspector.Label25, 0.5, 0.5);
   Set_Padding (Inspector.Label25, 0, 0);
   Set_Justify (Inspector.Label25, Justify_Center);
   Set_Line_Wrap (Inspector.Label25, False);
   Set_Tab (Inspector.Notebook4, 0, Inspector.Label25);

   Gtk_New (Inspector.Scrolledwindow5);
   Add (Inspector.Notebook4, Inspector.Scrolledwindow5);
   Set_Policy (Inspector.Scrolledwindow5, Policy_Automatic, Policy_Automatic);

   Gtk_New (Inspector.Clist2, 2);
   Add (Inspector.Scrolledwindow5, Inspector.Clist2);
   Set_Selection_Mode (Inspector.Clist2, Selection_Single);
   Set_Shadow_Type (Inspector.Clist2, Shadow_In);
   Set_Show_Titles (Inspector.Clist2, True);
   Set_Column_Width (Inspector.Clist2, 0, 80);
   Set_Column_Width (Inspector.Clist2, 1, 80);

   Gtk_New (Inspector.Label26, -("Signal"));
   Set_Alignment (Inspector.Label26, 0.5, 0.5);
   Set_Padding (Inspector.Label26, 0, 0);
   Set_Justify (Inspector.Label26, Justify_Center);
   Set_Line_Wrap (Inspector.Label26, False);
   Set_Column_Widget (Inspector.Clist2, 0, Inspector.Label26);

   Gtk_New (Inspector.Label27, -("Handler"));
   Set_Alignment (Inspector.Label27, 0.5, 0.5);
   Set_Padding (Inspector.Label27, 0, 0);
   Set_Justify (Inspector.Label27, Justify_Center);
   Set_Line_Wrap (Inspector.Label27, False);
   Set_Column_Widget (Inspector.Clist2, 1, Inspector.Label27);

   Gtk_New (Inspector.Label28, -("Signals"));
   Set_Alignment (Inspector.Label28, 0.5, 0.5);
   Set_Padding (Inspector.Label28, 0, 0);
   Set_Justify (Inspector.Label28, Justify_Center);
   Set_Line_Wrap (Inspector.Label28, False);
   Set_Tab (Inspector.Notebook4, 1, Inspector.Label28);

end Initialize;

end Inspector_Pkg;
