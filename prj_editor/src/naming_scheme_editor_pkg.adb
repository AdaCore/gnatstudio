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

pragma Style_Checks (Off);

with Gtk; use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Glide_Intl; use Glide_Intl;
with Naming_Scheme_Editor_Pkg.Callbacks; use Naming_Scheme_Editor_Pkg.Callbacks;

package body Naming_Scheme_Editor_Pkg is

procedure Gtk_New (Naming_Scheme_Editor : out Naming_Scheme_Editor_Access) is
begin
   Naming_Scheme_Editor := new Naming_Scheme_Editor_Record;
   Naming_Scheme_Editor_Pkg.Initialize (Naming_Scheme_Editor);
end Gtk_New;

procedure Initialize (Naming_Scheme_Editor : access Naming_Scheme_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Standard_Scheme_Items : String_List.Glist;
   Casing_Items : String_List.Glist;
   Spec_Extension_Items : String_List.Glist;
   Body_Extension_Items : String_List.Glist;
   Separate_Extension_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Naming_Scheme_Editor, Window_Toplevel);
   Set_Title (Naming_Scheme_Editor, -"Naming scheme");
   Set_Policy (Naming_Scheme_Editor, False, True, False);
   Set_Position (Naming_Scheme_Editor, Win_Pos_None);
   Set_Modal (Naming_Scheme_Editor, False);

   Gtk_New
     (Naming_Scheme_Editor.Alignment1, 0.5, 0.5, 1.0,
      1.0);
   Add (Naming_Scheme_Editor, Naming_Scheme_Editor.Alignment1);

   Gtk_New_Vbox (Naming_Scheme_Editor.Main_Box, False, 9);
   Set_Border_Width (Naming_Scheme_Editor.Main_Box, 4);
   Add (Naming_Scheme_Editor.Alignment1, Naming_Scheme_Editor.Main_Box);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox4, False, 0);
   Pack_Start (Naming_Scheme_Editor.Main_Box, Naming_Scheme_Editor.Hbox4, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Label_Naming_Scheme, -("Naming scheme:"));
   Set_Alignment (Naming_Scheme_Editor.Label_Naming_Scheme, 7.45058e-09, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label_Naming_Scheme, 10, 0);
   Set_Justify (Naming_Scheme_Editor.Label_Naming_Scheme, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label_Naming_Scheme, False);
   Pack_Start (Naming_Scheme_Editor.Hbox4, Naming_Scheme_Editor.Label_Naming_Scheme, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Standard_Scheme);
   Set_Case_Sensitive (Naming_Scheme_Editor.Standard_Scheme, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Standard_Scheme, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Standard_Scheme, False);
   String_List.Append (Standard_Scheme_Items, -"GNAT default");
   String_List.Append (Standard_Scheme_Items, -"unit.separate.1.ada");
   String_List.Append (Standard_Scheme_Items, -"unit__separate_.ada");
   String_List.Append (Standard_Scheme_Items, -"<custom>");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Standard_Scheme, Standard_Scheme_Items);
   Free_String_List (Standard_Scheme_Items);
   Pack_Start (Naming_Scheme_Editor.Hbox4, Naming_Scheme_Editor.Standard_Scheme, True, True, 0);

   Naming_Scheme_Editor.Combo_Entry3 := Get_Entry (Naming_Scheme_Editor.Standard_Scheme);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry3, False);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry3, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry3, -"GNAT default");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry3, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry3, "changed",
      Widget_Callback.To_Marshaller (On_Standard_Scheme_Changed'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Frame28, -"Details");
   Set_Shadow_Type (Naming_Scheme_Editor.Frame28, Shadow_Etched_In);
   Pack_Start (Naming_Scheme_Editor.Main_Box, Naming_Scheme_Editor.Frame28, False, False, 0);

   Gtk_New_Vbox (Naming_Scheme_Editor.Vbox53, False, 0);
   Add (Naming_Scheme_Editor.Frame28, Naming_Scheme_Editor.Vbox53);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox5, False, 0);
   Pack_Start (Naming_Scheme_Editor.Vbox53, Naming_Scheme_Editor.Hbox5, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Label_Casing, -("Filename casing:"));
   Set_Alignment (Naming_Scheme_Editor.Label_Casing, 7.45058e-09, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label_Casing, 10, 0);
   Set_Justify (Naming_Scheme_Editor.Label_Casing, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label_Casing, False);
   Pack_Start (Naming_Scheme_Editor.Hbox5, Naming_Scheme_Editor.Label_Casing, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Casing);
   Set_Case_Sensitive (Naming_Scheme_Editor.Casing, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Casing, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Casing, False);
   String_List.Append (Casing_Items, -"");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Casing, Casing_Items);
   Free_String_List (Casing_Items);
   Pack_Start (Naming_Scheme_Editor.Hbox5, Naming_Scheme_Editor.Casing, True, True, 0);

   Naming_Scheme_Editor.Combo_Entry2 := Get_Entry (Naming_Scheme_Editor.Casing);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry2, False);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry2, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry2, -"");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry2, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry2, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox7, False, 0);
   Pack_Start (Naming_Scheme_Editor.Vbox53, Naming_Scheme_Editor.Hbox7, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Label_Dot_Replacement, -("Dot replacement:"));
   Set_Alignment (Naming_Scheme_Editor.Label_Dot_Replacement, 7.45058e-09, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label_Dot_Replacement, 10, 0);
   Set_Justify (Naming_Scheme_Editor.Label_Dot_Replacement, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label_Dot_Replacement, False);
   Pack_Start (Naming_Scheme_Editor.Hbox7, Naming_Scheme_Editor.Label_Dot_Replacement, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Dot_Replacement);
   Set_Editable (Naming_Scheme_Editor.Dot_Replacement, True);
   Set_Max_Length (Naming_Scheme_Editor.Dot_Replacement, 0);
   Set_Text (Naming_Scheme_Editor.Dot_Replacement, -"-");
   Set_Visibility (Naming_Scheme_Editor.Dot_Replacement, True);
   Pack_Start (Naming_Scheme_Editor.Hbox7, Naming_Scheme_Editor.Dot_Replacement, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Dot_Replacement, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox6, False, 0);
   Pack_Start (Naming_Scheme_Editor.Vbox53, Naming_Scheme_Editor.Hbox6, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Label_Spec_Extensions, -("Spec extensions:"));
   Set_Alignment (Naming_Scheme_Editor.Label_Spec_Extensions, 7.45058e-09, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label_Spec_Extensions, 10, 0);
   Set_Justify (Naming_Scheme_Editor.Label_Spec_Extensions, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label_Spec_Extensions, False);
   Pack_Start (Naming_Scheme_Editor.Hbox6, Naming_Scheme_Editor.Label_Spec_Extensions, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Spec_Extension);
   Set_Case_Sensitive (Naming_Scheme_Editor.Spec_Extension, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Spec_Extension, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Spec_Extension, False);
   String_List.Append (Spec_Extension_Items, -".ads");
   String_List.Append (Spec_Extension_Items, -".1.ada");
   String_List.Append (Spec_Extension_Items, -"_.ada");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Spec_Extension, Spec_Extension_Items);
   Free_String_List (Spec_Extension_Items);
   Pack_Start (Naming_Scheme_Editor.Hbox6, Naming_Scheme_Editor.Spec_Extension, True, True, 0);

   Naming_Scheme_Editor.Combo_Entry4 := Get_Entry (Naming_Scheme_Editor.Spec_Extension);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry4, True);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry4, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry4, -".ads");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry4, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry4, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox8, False, 0);
   Pack_Start (Naming_Scheme_Editor.Vbox53, Naming_Scheme_Editor.Hbox8, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Label_Body_Extensions, -("Body extensions:"));
   Set_Alignment (Naming_Scheme_Editor.Label_Body_Extensions, 0.0, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label_Body_Extensions, 10, 0);
   Set_Justify (Naming_Scheme_Editor.Label_Body_Extensions, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label_Body_Extensions, False);
   Pack_Start (Naming_Scheme_Editor.Hbox8, Naming_Scheme_Editor.Label_Body_Extensions, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Body_Extension);
   Set_Case_Sensitive (Naming_Scheme_Editor.Body_Extension, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Body_Extension, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Body_Extension, False);
   String_List.Append (Body_Extension_Items, -".adb");
   String_List.Append (Body_Extension_Items, -".2.ada");
   String_List.Append (Body_Extension_Items, -".ada");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Body_Extension, Body_Extension_Items);
   Free_String_List (Body_Extension_Items);
   Pack_Start (Naming_Scheme_Editor.Hbox8, Naming_Scheme_Editor.Body_Extension, True, True, 0);

   Naming_Scheme_Editor.Combo_Entry5 := Get_Entry (Naming_Scheme_Editor.Body_Extension);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry5, True);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry5, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry5, -".adb");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry5, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry5, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox9, False, 0);
   Pack_Start (Naming_Scheme_Editor.Vbox53, Naming_Scheme_Editor.Hbox9, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Label_Separate_Extensions, -("Separate extensions:"));
   Set_Alignment (Naming_Scheme_Editor.Label_Separate_Extensions, 7.45058e-09, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label_Separate_Extensions, 10, 0);
   Set_Justify (Naming_Scheme_Editor.Label_Separate_Extensions, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label_Separate_Extensions, False);
   Pack_Start (Naming_Scheme_Editor.Hbox9, Naming_Scheme_Editor.Label_Separate_Extensions, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Separate_Extension);
   Set_Case_Sensitive (Naming_Scheme_Editor.Separate_Extension, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Separate_Extension, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Separate_Extension, False);
   String_List.Append (Separate_Extension_Items, -".adb");
   String_List.Append (Separate_Extension_Items, -".2.ada");
   String_List.Append (Separate_Extension_Items, -".ada");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Separate_Extension, Separate_Extension_Items);
   Free_String_List (Separate_Extension_Items);
   Pack_Start (Naming_Scheme_Editor.Hbox9, Naming_Scheme_Editor.Separate_Extension, True, True, 0);

   Naming_Scheme_Editor.Combo_Entry6 := Get_Entry (Naming_Scheme_Editor.Separate_Extension);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry6, True);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry6, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry6, -".adb");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry6, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry6, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Frame30, -"Exceptions");
   Set_Shadow_Type (Naming_Scheme_Editor.Frame30, Shadow_Etched_In);
   Pack_Start (Naming_Scheme_Editor.Main_Box, Naming_Scheme_Editor.Frame30, True, True, 0);

   Gtk_New_Vbox (Naming_Scheme_Editor.Vbox29, False, 0);
   Set_Border_Width (Naming_Scheme_Editor.Vbox29, 3);
   Add (Naming_Scheme_Editor.Frame30, Naming_Scheme_Editor.Vbox29);

   Gtk_New (Naming_Scheme_Editor.Scrolledwindow1);
   Set_Policy (Naming_Scheme_Editor.Scrolledwindow1, Policy_Automatic, Policy_Automatic);
   Pack_Start (Naming_Scheme_Editor.Vbox29, Naming_Scheme_Editor.Scrolledwindow1, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Exception_List, 3);
   Set_Selection_Mode (Naming_Scheme_Editor.Exception_List, Selection_Single);
   Set_Shadow_Type (Naming_Scheme_Editor.Exception_List, Shadow_In);
   Set_Show_Titles (Naming_Scheme_Editor.Exception_List, True);
   Set_Column_Width (Naming_Scheme_Editor.Exception_List, 0, 80);
   Set_Column_Width (Naming_Scheme_Editor.Exception_List, 1, 124);
   Set_Column_Width (Naming_Scheme_Editor.Exception_List, 2, 80);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Exception_List, "select_row", On_Exceptions_List_Select_Row'Access, Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Exception_List, "key_press_event", On_Exception_List_Key_Press_Event'Access, Naming_Scheme_Editor);
   Add (Naming_Scheme_Editor.Scrolledwindow1, Naming_Scheme_Editor.Exception_List);

   Gtk_New (Naming_Scheme_Editor.Label28, -("Unit name"));
   Set_Alignment (Naming_Scheme_Editor.Label28, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label28, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label28, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label28, False);
   Set_Column_Widget (Naming_Scheme_Editor.Exception_List, 0, Naming_Scheme_Editor.Label28);

   Gtk_New (Naming_Scheme_Editor.Label29, -("Spec filename"));
   Set_Alignment (Naming_Scheme_Editor.Label29, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label29, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label29, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label29, False);
   Set_Column_Widget (Naming_Scheme_Editor.Exception_List, 1, Naming_Scheme_Editor.Label29);

   Gtk_New (Naming_Scheme_Editor.Label30, -("Body filename"));
   Set_Alignment (Naming_Scheme_Editor.Label30, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label30, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label30, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label30, False);
   Set_Column_Widget (Naming_Scheme_Editor.Exception_List, 2, Naming_Scheme_Editor.Label30);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox3, False, 3);
   Pack_Start (Naming_Scheme_Editor.Vbox29, Naming_Scheme_Editor.Hbox3, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Unit_Name_Entry);
   Set_Editable (Naming_Scheme_Editor.Unit_Name_Entry, True);
   Set_Max_Length (Naming_Scheme_Editor.Unit_Name_Entry, 0);
   Set_Text (Naming_Scheme_Editor.Unit_Name_Entry, -"");
   Set_Visibility (Naming_Scheme_Editor.Unit_Name_Entry, True);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Unit_Name_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Unit_Name_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Unit_Name_Entry, "key_press_event", On_Unit_Name_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Spec_Filename_Entry);
   Set_Editable (Naming_Scheme_Editor.Spec_Filename_Entry, True);
   Set_Max_Length (Naming_Scheme_Editor.Spec_Filename_Entry, 0);
   Set_Text (Naming_Scheme_Editor.Spec_Filename_Entry, -"");
   Set_Visibility (Naming_Scheme_Editor.Spec_Filename_Entry, True);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Spec_Filename_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Spec_Filename_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Spec_Filename_Entry, "key_press_event", On_Spec_Filename_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Body_Filename_Entry);
   Set_Editable (Naming_Scheme_Editor.Body_Filename_Entry, True);
   Set_Max_Length (Naming_Scheme_Editor.Body_Filename_Entry, 0);
   Set_Text (Naming_Scheme_Editor.Body_Filename_Entry, -"");
   Set_Visibility (Naming_Scheme_Editor.Body_Filename_Entry, True);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Body_Filename_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Body_Filename_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Body_Filename_Entry, "key_press_event", On_Body_Filename_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Update, -"Update");
   Set_Relief (Naming_Scheme_Editor.Update, Relief_Normal);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Update, False, False, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Update, "clicked",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);

end Initialize;

end Naming_Scheme_Editor_Pkg;
