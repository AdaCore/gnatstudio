with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Codefix_Interface; use Callbacks_Codefix_Interface;
with Codefix_Interface_Intl; use Codefix_Interface_Intl;
with Codefix_Window_Pkg.Callbacks; use Codefix_Window_Pkg.Callbacks;

package body Codefix_Window_Pkg is

procedure Gtk_New (Codefix_Window : out Codefix_Window_Access) is
begin
   Codefix_Window := new Codefix_Window_Record;
   Codefix_Window_Pkg.Initialize (Codefix_Window);
end Gtk_New;

procedure Initialize (Codefix_Window : access Codefix_Window_Record'Class) is
   pragma Suppress (All_Checks);
   Fix_Caption_List_Items : String_List.Glist;
   Tooltips : Gtk_Tooltips;

begin
   Gtk.Window.Initialize (Codefix_Window, Window_Toplevel);
   Set_Title (Codefix_Window, -"Codefix");
   Set_Policy (Codefix_Window, False, True, False);
   Set_Position (Codefix_Window, Win_Pos_None);
   Set_Modal (Codefix_Window, False);
   Set_USize (Codefix_Window, 640, 480);
   Return_Callback.Connect
     (Codefix_Window, "delete_event", On_Codefix_Window_Delete_Event'Access);

   Gtk_New_Hbox (Codefix_Window.Hbox1, False, 0);
   Add (Codefix_Window, Codefix_Window.Hbox1);

   Gtk_New_Vbox (Codefix_Window.Vbox6, False, 0);
   Pack_Start (Codefix_Window.Hbox1, Codefix_Window.Vbox6, True, True, 0);

   Gtk_New (Codefix_Window.Error_Caption, -("Error_Caption"));
   Set_Alignment (Codefix_Window.Error_Caption, 0.5, 0.5);
   Set_Padding (Codefix_Window.Error_Caption, 0, 0);
   Set_Justify (Codefix_Window.Error_Caption, Justify_Left);
   Set_Line_Wrap (Codefix_Window.Error_Caption, False);
   Pack_Start (Codefix_Window.Vbox6, Codefix_Window.Error_Caption, False, False, 0);

   Gtk_New (Codefix_Window.Choices_Proposed);
   Set_Scrollable (Codefix_Window.Choices_Proposed, False);
   Set_Show_Border (Codefix_Window.Choices_Proposed, True);
   Set_Show_Tabs (Codefix_Window.Choices_Proposed, False);
   Set_Tab_Hborder (Codefix_Window.Choices_Proposed, 2);
   Set_Tab_Vborder (Codefix_Window.Choices_Proposed, 2);
   Set_Tab_Pos (Codefix_Window.Choices_Proposed, Pos_Top);
   Pack_Start (Codefix_Window.Vbox6, Codefix_Window.Choices_Proposed, True, True, 0);

   Gtk_New_Vbox (Codefix_Window.Vbox4, False, 0);
   Add (Codefix_Window.Choices_Proposed, Codefix_Window.Vbox4);

   Gtk_New (Codefix_Window.Label3, -("Choice 1"));
   Set_Alignment (Codefix_Window.Label3, 0.5, 0.5);
   Set_Padding (Codefix_Window.Label3, 0, 0);
   Set_Justify (Codefix_Window.Label3, Justify_Center);
   Set_Line_Wrap (Codefix_Window.Label3, False);
   Set_Tab (Codefix_Window.Choices_Proposed, 0, Codefix_Window.Label3);

   Gtk_New (Codefix_Window.Fix_Caption_List);
   Set_Case_Sensitive (Codefix_Window.Fix_Caption_List, False);
   Set_Use_Arrows (Codefix_Window.Fix_Caption_List, True);
   Set_Use_Arrows_Always (Codefix_Window.Fix_Caption_List, False);
   String_List.Append (Fix_Caption_List_Items, -"");
   Combo.Set_Popdown_Strings (Codefix_Window.Fix_Caption_List, Fix_Caption_List_Items);
   Free_String_List (Fix_Caption_List_Items);
   Pack_Start (Codefix_Window.Vbox6, Codefix_Window.Fix_Caption_List, False, False, 0);

   Codefix_Window.Fix_Entry := Get_Entry (Codefix_Window.Fix_Caption_List);
   Set_Editable (Codefix_Window.Fix_Entry, True);
   Set_Max_Length (Codefix_Window.Fix_Entry, 0);
   Set_Text (Codefix_Window.Fix_Entry, -"");
   Set_Visibility (Codefix_Window.Fix_Entry, True);
   Widget_Callback.Object_Connect
     (Codefix_Window.Fix_Entry, "changed",
      Widget_Callback.To_Marshaller (On_Fix_Entry_Changed'Access), Codefix_Window);

   Gtk_New_Vbox (Codefix_Window.Vbox5, False, 0);
   Pack_Start (Codefix_Window.Hbox1, Codefix_Window.Vbox5, False, True, 0);

   Gtk_New (Codefix_Window.Vbuttonbox1);
   Set_Spacing (Codefix_Window.Vbuttonbox1, 10);
   Set_Layout (Codefix_Window.Vbuttonbox1, Buttonbox_Start);
   Set_Child_Size (Codefix_Window.Vbuttonbox1, 85, 27);
   Set_Child_Ipadding (Codefix_Window.Vbuttonbox1, 7, 0);
   Pack_Start (Codefix_Window.Vbox5, Codefix_Window.Vbuttonbox1, True, True, 5);

   Gtk_New
     (Codefix_Window.Alignment1, 0.5, 0.5, 1.0, 
      1.0);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Alignment1);

   Gtk_New (Codefix_Window.Skip_Correction, -"Skip");
   Set_Flags (Codefix_Window.Skip_Correction, Can_Default);
   Gtk_New (Tooltips);
   Set_Tip (Tooltips, Codefix_Window.Skip_Correction, -"Don't fix this error");
   Widget_Callback.Object_Connect
     (Codefix_Window.Skip_Correction, "clicked",
      Widget_Callback.To_Marshaller (On_Skip_Correction_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Alignment1, Codefix_Window.Skip_Correction);

   Gtk_New (Codefix_Window.Accept_Correction);
   Set_Flags (Codefix_Window.Accept_Correction, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Accept_Correction, -"Fix this error");
   Widget_Callback.Object_Connect
     (Codefix_Window.Accept_Correction, "clicked",
      Widget_Callback.To_Marshaller (On_Accept_Correction_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Accept_Correction);

   Gtk_New (Codefix_Window.Skip_All_Corrections, -"Skip all");
   Set_Flags (Codefix_Window.Skip_All_Corrections, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Skip_All_Corrections, -"Don't fix any error of the same category than the current one");
   Widget_Callback.Object_Connect
     (Codefix_Window.Skip_All_Corrections, "clicked",
      Widget_Callback.To_Marshaller (On_Skip_All_Corrections_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Skip_All_Corrections);

   Gtk_New (Codefix_Window.Accept_All_Corrections, -"Apply all");
   Set_Flags (Codefix_Window.Accept_All_Corrections, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Accept_All_Corrections, -"Fix each error of the same category than the current one");
   Widget_Callback.Object_Connect
     (Codefix_Window.Accept_All_Corrections, "clicked",
      Widget_Callback.To_Marshaller (On_Accept_All_Corrections_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Accept_All_Corrections);

   Gtk_New (Codefix_Window.Undo);
   Set_Flags (Codefix_Window.Undo, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Undo, -"Cancel previous changes");
   Widget_Callback.Object_Connect
     (Codefix_Window.Undo, "clicked",
      Widget_Callback.To_Marshaller (On_Undo_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Undo);

   Gtk_New (Codefix_Window.Refresh, -"Refresh");
   Set_Flags (Codefix_Window.Refresh, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Refresh, -"Recalculate the fix with last user's changes");
   Widget_Callback.Object_Connect
     (Codefix_Window.Refresh, "clicked",
      Widget_Callback.To_Marshaller (On_Refresh_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox1, Codefix_Window.Refresh);

   Gtk_New (Codefix_Window.Vbuttonbox2);
   Set_Spacing (Codefix_Window.Vbuttonbox2, 10);
   Set_Layout (Codefix_Window.Vbuttonbox2, Buttonbox_End);
   Set_Child_Size (Codefix_Window.Vbuttonbox2, 85, 27);
   Set_Child_Ipadding (Codefix_Window.Vbuttonbox2, 7, 0);
   Pack_Start (Codefix_Window.Vbox5, Codefix_Window.Vbuttonbox2, True, True, 5);

   Gtk_New (Codefix_Window.Cancel_Changes);
   Set_Flags (Codefix_Window.Cancel_Changes, Can_Default);
   Set_Tip (Tooltips, Codefix_Window.Cancel_Changes, -"Cancel all changes");
   Widget_Callback.Object_Connect
     (Codefix_Window.Cancel_Changes, "clicked",
      Widget_Callback.To_Marshaller (On_Cancel_Changes_Clicked'Access), Codefix_Window);
   Add (Codefix_Window.Vbuttonbox2, Codefix_Window.Cancel_Changes);

end Initialize;

end Codefix_Window_Pkg;
