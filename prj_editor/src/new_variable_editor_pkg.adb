-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Glide_Intl; use Glide_Intl;
with New_Variable_Editor_Pkg.Callbacks; use New_Variable_Editor_Pkg.Callbacks;

package body New_Variable_Editor_Pkg is

procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access) is
begin
   New_Variable_Editor := new New_Variable_Editor_Record;
   New_Variable_Editor_Pkg.Initialize (New_Variable_Editor);
end Gtk_New;

procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Default_Env_Variable_Items : String_List.Glist;
   List_Env_Variables_Items : String_List.Glist;
   Vbox39_Group : Widget_SList.GSList;

begin
   Gtk.Window.Initialize (New_Variable_Editor, Window_Toplevel);
   Set_Title (New_Variable_Editor, -"Creating a variable");
   Set_Policy (New_Variable_Editor, True, True, False);
   Set_Position (New_Variable_Editor, Win_Pos_Center);
   Set_Modal (New_Variable_Editor, False);
   Set_Default_Size (New_Variable_Editor, 500, 480);

   Gtk_New_Vbox (New_Variable_Editor.Vbox37, False, 0);
   Add (New_Variable_Editor, New_Variable_Editor.Vbox37);

   Gtk_New (New_Variable_Editor.Name_Frame, -"Name");
   Set_Border_Width (New_Variable_Editor.Name_Frame, 10);
   Set_Shadow_Type (New_Variable_Editor.Name_Frame, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Name_Frame, False, False, 0);

   Gtk_New (New_Variable_Editor.Variable_Name);
   Set_Editable (New_Variable_Editor.Variable_Name, True);
   Set_Max_Length (New_Variable_Editor.Variable_Name, 0);
   Set_Text (New_Variable_Editor.Variable_Name, -"");
   Set_Visibility (New_Variable_Editor.Variable_Name, True);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Variable_Name, "changed",
      Widget_Callback.To_Marshaller (On_Variable_Name_Changed'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Name_Frame, New_Variable_Editor.Variable_Name);

   Gtk_New (New_Variable_Editor.Frame33, -"Importing");
   Set_Border_Width (New_Variable_Editor.Frame33, 10);
   Set_Shadow_Type (New_Variable_Editor.Frame33, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Frame33, False, False, 0);

   Gtk_New_Vbox (New_Variable_Editor.Vbox38, False, 0);
   Add (New_Variable_Editor.Frame33, New_Variable_Editor.Vbox38);

   Gtk_New (New_Variable_Editor.Get_Environment, -"Get value from environment");
   Set_Active (New_Variable_Editor.Get_Environment, False);
   Pack_Start (New_Variable_Editor.Vbox38, New_Variable_Editor.Get_Environment, False, False, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Get_Environment, "toggled",
      Widget_Callback.To_Marshaller (On_Get_Environment_Toggled'Access), New_Variable_Editor);

   Gtk_New
     (New_Variable_Editor.Alignment7, 1.0, 0.5, 0.9,
      0.9);
   Pack_Start (New_Variable_Editor.Vbox38, New_Variable_Editor.Alignment7, False, False, 0);

   Gtk_New (New_Variable_Editor.Environment_Table, 3, 2, False);
   Set_Row_Spacings (New_Variable_Editor.Environment_Table, 0);
   Set_Col_Spacings (New_Variable_Editor.Environment_Table, 9);
   Add (New_Variable_Editor.Alignment7, New_Variable_Editor.Environment_Table);

   Gtk_New (New_Variable_Editor.Default_Value_Label, -("Default value:"));
   Set_Alignment (New_Variable_Editor.Default_Value_Label, 7.45058e-09, 0.5);
   Set_Padding (New_Variable_Editor.Default_Value_Label, 0, 0);
   Set_Justify (New_Variable_Editor.Default_Value_Label, Justify_Center);
   Set_Line_Wrap (New_Variable_Editor.Default_Value_Label, False);
   Attach (New_Variable_Editor.Environment_Table, New_Variable_Editor.Default_Value_Label, 0, 1, 2, 3,
     Shrink or Fill, 0,
     0, 0);

   Gtk_New (New_Variable_Editor.Default_Env_Variable);
   Set_Case_Sensitive (New_Variable_Editor.Default_Env_Variable, False);
   Set_Use_Arrows (New_Variable_Editor.Default_Env_Variable, True);
   Set_Use_Arrows_Always (New_Variable_Editor.Default_Env_Variable, False);
   String_List.Append (Default_Env_Variable_Items, -"");
   Combo.Set_Popdown_Strings (New_Variable_Editor.Default_Env_Variable, Default_Env_Variable_Items);
   Free_String_List (Default_Env_Variable_Items);
   Attach (New_Variable_Editor.Environment_Table, New_Variable_Editor.Default_Env_Variable, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);

   New_Variable_Editor.Combo_Entry8 := Get_Entry (New_Variable_Editor.Default_Env_Variable);
   Set_Editable (New_Variable_Editor.Combo_Entry8, False);
   Set_Max_Length (New_Variable_Editor.Combo_Entry8, 0);
   Set_Text (New_Variable_Editor.Combo_Entry8, -"");
   Set_Visibility (New_Variable_Editor.Combo_Entry8, True);

   Gtk_New (New_Variable_Editor.List_Env_Variables);
   Set_Case_Sensitive (New_Variable_Editor.List_Env_Variables, False);
   Set_Use_Arrows (New_Variable_Editor.List_Env_Variables, True);
   Set_Use_Arrows_Always (New_Variable_Editor.List_Env_Variables, False);
   String_List.Append (List_Env_Variables_Items, -"");
   Combo.Set_Popdown_Strings (New_Variable_Editor.List_Env_Variables, List_Env_Variables_Items);
   Free_String_List (List_Env_Variables_Items);
   Attach (New_Variable_Editor.Environment_Table, New_Variable_Editor.List_Env_Variables, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);

   New_Variable_Editor.Combo_Entry7 := Get_Entry (New_Variable_Editor.List_Env_Variables);
   Set_Editable (New_Variable_Editor.Combo_Entry7, True);
   Set_Max_Length (New_Variable_Editor.Combo_Entry7, 0);
   Set_Text (New_Variable_Editor.Combo_Entry7, -"");
   Set_Visibility (New_Variable_Editor.Combo_Entry7, True);

   Gtk_New (New_Variable_Editor.Label55, -("Name:"));
   Set_Alignment (New_Variable_Editor.Label55, 7.45058e-09, 0.5);
   Set_Padding (New_Variable_Editor.Label55, 0, 0);
   Set_Justify (New_Variable_Editor.Label55, Justify_Center);
   Set_Line_Wrap (New_Variable_Editor.Label55, False);
   Attach (New_Variable_Editor.Environment_Table, New_Variable_Editor.Label55, 0, 1, 1, 2,
     Shrink or Fill, 0,
     0, 0);

   Gtk_New (New_Variable_Editor.Env_Must_Be_Defined, -"Environment variable must be defined");
   Set_Active (New_Variable_Editor.Env_Must_Be_Defined, False);
   Attach (New_Variable_Editor.Environment_Table, New_Variable_Editor.Env_Must_Be_Defined, 0, 2, 0, 1,
     Expand or Shrink or Fill, 0,
     0, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Env_Must_Be_Defined, "toggled",
      Widget_Callback.To_Marshaller (On_Env_Must_Be_Defined_Toggled'Access), New_Variable_Editor);

   Gtk_New (New_Variable_Editor.Frame34, -"Type and current value");
   Set_Border_Width (New_Variable_Editor.Frame34, 10);
   Set_Shadow_Type (New_Variable_Editor.Frame34, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Frame34, True, True, 0);

   Gtk_New_Vbox (New_Variable_Editor.Vbox39, False, 0);
   Add (New_Variable_Editor.Frame34, New_Variable_Editor.Vbox39);

   Gtk_New (New_Variable_Editor.Typed_Variable, Vbox39_Group, -"Enumeration of possible values");
   Vbox39_Group := Group (New_Variable_Editor.Typed_Variable);
   Set_Active (New_Variable_Editor.Typed_Variable, False);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Typed_Variable, False, False, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Typed_Variable, "toggled",
      Widget_Callback.To_Marshaller (On_Typed_Variable_Toggled'Access), New_Variable_Editor);

   Gtk_New
     (New_Variable_Editor.Alignment4, 1.0, 0.5, 0.9,
      0.9);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Alignment4, True, True, 0);

   Gtk_New (New_Variable_Editor.Enumeration_Scrolled);
   Set_Policy (New_Variable_Editor.Enumeration_Scrolled, Policy_Never, Policy_Always);
   Add (New_Variable_Editor.Alignment4, New_Variable_Editor.Enumeration_Scrolled);

   Gtk_New (New_Variable_Editor.Enumeration_Value);
   Set_Editable (New_Variable_Editor.Enumeration_Value, True);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Enumeration_Value, "changed",
      Widget_Callback.To_Marshaller (On_Enumeration_Value_Changed'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Enumeration_Scrolled, New_Variable_Editor.Enumeration_Value);

   Gtk_New (New_Variable_Editor.Untyped_List_Variable, Vbox39_Group, -"Any list value authorized");
   Vbox39_Group := Group (New_Variable_Editor.Untyped_List_Variable);
   Set_Active (New_Variable_Editor.Untyped_List_Variable, False);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Untyped_List_Variable, False, False, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Untyped_List_Variable, "toggled",
      Widget_Callback.To_Marshaller (On_Typed_Variable_Toggled'Access), New_Variable_Editor);

   Gtk_New
     (New_Variable_Editor.Untyped_Alignment, 1.0, 0.5, 0.9,
      0.9);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Untyped_Alignment, True, True, 0);

   Gtk_New (New_Variable_Editor.List_Scrolled);
   Set_Policy (New_Variable_Editor.List_Scrolled, Policy_Never, Policy_Always);
   Add (New_Variable_Editor.Untyped_Alignment, New_Variable_Editor.List_Scrolled);

   Gtk_New (New_Variable_Editor.List_Value);
   Set_Editable (New_Variable_Editor.List_Value, True);
   Add (New_Variable_Editor.List_Scrolled, New_Variable_Editor.List_Value);

   Gtk_New (New_Variable_Editor.Untyped_Single_Variable, Vbox39_Group, -"Any string value authorized");
   Vbox39_Group := Group (New_Variable_Editor.Untyped_Single_Variable);
   Set_Active (New_Variable_Editor.Untyped_Single_Variable, False);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Untyped_Single_Variable, False, False, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Untyped_Single_Variable, "toggled",
      Widget_Callback.To_Marshaller (On_Typed_Variable_Toggled'Access), New_Variable_Editor);

   Gtk_New
     (New_Variable_Editor.Single_Alignment, 1.0, 0.5, 0.9,
      0.9);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Single_Alignment, False, False, 0);

   Gtk_New (New_Variable_Editor.Single_Value);
   Set_Editable (New_Variable_Editor.Single_Value, True);
   Add (New_Variable_Editor.Single_Alignment, New_Variable_Editor.Single_Value);

   Gtk_New_Hseparator (New_Variable_Editor.Hseparator4);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Hseparator4, False, False, 0);

   Gtk_New (New_Variable_Editor.Hbuttonbox3);
   Set_Spacing (New_Variable_Editor.Hbuttonbox3, 30);
   Set_Layout (New_Variable_Editor.Hbuttonbox3, Buttonbox_Spread);
   Set_Child_Size (New_Variable_Editor.Hbuttonbox3, 85, 27);
   Set_Child_Ipadding (New_Variable_Editor.Hbuttonbox3, 7, 0);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Hbuttonbox3, False, False, 0);

   Gtk_New (New_Variable_Editor.Add_Button, -"Add");
   Set_Relief (New_Variable_Editor.Add_Button, Relief_Normal);
   Set_Flags (New_Variable_Editor.Add_Button, Can_Default);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Add_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Clicked'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Hbuttonbox3, New_Variable_Editor.Add_Button);

   Gtk_New (New_Variable_Editor.Cancel_Button, -"Cancel");
   Set_Relief (New_Variable_Editor.Cancel_Button, Relief_Normal);
   Set_Flags (New_Variable_Editor.Cancel_Button, Can_Default);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Cancel_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Cancel_Clicked'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Hbuttonbox3, New_Variable_Editor.Cancel_Button);

end Initialize;

end New_Variable_Editor_Pkg;
