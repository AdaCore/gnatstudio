-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Gtk; use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Glide_Intl;      use Glide_Intl;
with Foreign_Naming_Scheme_Editor_Pkg.Callbacks; use Foreign_Naming_Scheme_Editor_Pkg.Callbacks;

package body Foreign_Naming_Scheme_Editor_Pkg is

procedure Gtk_New (Foreign_Naming_Scheme_Editor : out Foreign_Naming_Scheme_Editor_Access) is
begin
   Foreign_Naming_Scheme_Editor := new Foreign_Naming_Scheme_Editor_Record;
   Foreign_Naming_Scheme_Editor_Pkg.Initialize (Foreign_Naming_Scheme_Editor);
end Gtk_New;

procedure Initialize (Foreign_Naming_Scheme_Editor : access Foreign_Naming_Scheme_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Header_File_Extension_Items : String_List.Glist;
   Implementation_Extension_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Foreign_Naming_Scheme_Editor, Window_Toplevel);
   Set_Title (Foreign_Naming_Scheme_Editor, -"Naming schem");
   Set_Policy (Foreign_Naming_Scheme_Editor, False, True, True);
   Set_Position (Foreign_Naming_Scheme_Editor, Win_Pos_None);
   Set_Modal (Foreign_Naming_Scheme_Editor, False);

   Gtk_New_Vbox (Foreign_Naming_Scheme_Editor.Main_Box, False, 0);
   Add (Foreign_Naming_Scheme_Editor, Foreign_Naming_Scheme_Editor.Main_Box);

   Gtk_New (Foreign_Naming_Scheme_Editor.Frame46, -"Details");
   Set_Shadow_Type (Foreign_Naming_Scheme_Editor.Frame46, Shadow_Etched_In);
   Pack_Start (Foreign_Naming_Scheme_Editor.Main_Box, Foreign_Naming_Scheme_Editor.Frame46, False, True, 0);

   Gtk_New (Foreign_Naming_Scheme_Editor.Table6, 2, 2, False);
   Set_Row_Spacings (Foreign_Naming_Scheme_Editor.Table6, 0);
   Set_Col_Spacings (Foreign_Naming_Scheme_Editor.Table6, 0);
   Add (Foreign_Naming_Scheme_Editor.Frame46, Foreign_Naming_Scheme_Editor.Table6);

   Gtk_New (Foreign_Naming_Scheme_Editor.Label75, -("Header files:"));
   Set_Alignment (Foreign_Naming_Scheme_Editor.Label75, 0.0, 0.5);
   Set_Padding (Foreign_Naming_Scheme_Editor.Label75, 0, 0);
   Set_Justify (Foreign_Naming_Scheme_Editor.Label75, Justify_Center);
   Set_Line_Wrap (Foreign_Naming_Scheme_Editor.Label75, False);
   Attach (Foreign_Naming_Scheme_Editor.Table6, Foreign_Naming_Scheme_Editor.Label75, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Foreign_Naming_Scheme_Editor.Label76, -("Implementation:"));
   Set_Alignment (Foreign_Naming_Scheme_Editor.Label76, 0.0, 0.5);
   Set_Padding (Foreign_Naming_Scheme_Editor.Label76, 0, 0);
   Set_Justify (Foreign_Naming_Scheme_Editor.Label76, Justify_Center);
   Set_Line_Wrap (Foreign_Naming_Scheme_Editor.Label76, False);
   Attach (Foreign_Naming_Scheme_Editor.Table6, Foreign_Naming_Scheme_Editor.Label76, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Foreign_Naming_Scheme_Editor.Header_File_Extension);
   Set_Case_Sensitive (Foreign_Naming_Scheme_Editor.Header_File_Extension, True);
   Set_Use_Arrows (Foreign_Naming_Scheme_Editor.Header_File_Extension, True);
   Set_Use_Arrows_Always (Foreign_Naming_Scheme_Editor.Header_File_Extension, False);
   String_List.Append (Header_File_Extension_Items, -".h");
   String_List.Append (Header_File_Extension_Items, -".hh");
   String_List.Append (Header_File_Extension_Items, -".H");
   String_List.Append (Header_File_Extension_Items, -".hpp");
   Combo.Set_Popdown_Strings (Foreign_Naming_Scheme_Editor.Header_File_Extension, Header_File_Extension_Items);
   Free_String_List (Header_File_Extension_Items);
   Attach (Foreign_Naming_Scheme_Editor.Table6, Foreign_Naming_Scheme_Editor.Header_File_Extension, 1, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Foreign_Naming_Scheme_Editor.Combo_Entry10 := Get_Entry (Foreign_Naming_Scheme_Editor.Header_File_Extension);
   Set_Editable (Foreign_Naming_Scheme_Editor.Combo_Entry10, True);
   Set_Max_Length (Foreign_Naming_Scheme_Editor.Combo_Entry10, 0);
   Set_Text (Foreign_Naming_Scheme_Editor.Combo_Entry10, -".h");
   Set_Visibility (Foreign_Naming_Scheme_Editor.Combo_Entry10, True);

   Gtk_New (Foreign_Naming_Scheme_Editor.Implementation_Extension);
   Set_Case_Sensitive (Foreign_Naming_Scheme_Editor.Implementation_Extension, True);
   Set_Use_Arrows (Foreign_Naming_Scheme_Editor.Implementation_Extension, True);
   Set_Use_Arrows_Always (Foreign_Naming_Scheme_Editor.Implementation_Extension, False);
   String_List.Append (Implementation_Extension_Items, -".c");
   String_List.Append (Implementation_Extension_Items, -".cc");
   String_List.Append (Implementation_Extension_Items, -".C");
   String_List.Append (Implementation_Extension_Items, -".cpp");
   Combo.Set_Popdown_Strings (Foreign_Naming_Scheme_Editor.Implementation_Extension, Implementation_Extension_Items);
   Free_String_List (Implementation_Extension_Items);
   Attach (Foreign_Naming_Scheme_Editor.Table6, Foreign_Naming_Scheme_Editor.Implementation_Extension, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Foreign_Naming_Scheme_Editor.Combo_Entry11 := Get_Entry (Foreign_Naming_Scheme_Editor.Implementation_Extension);
   Set_Editable (Foreign_Naming_Scheme_Editor.Combo_Entry11, True);
   Set_Max_Length (Foreign_Naming_Scheme_Editor.Combo_Entry11, 0);
   Set_Text (Foreign_Naming_Scheme_Editor.Combo_Entry11, -".c");
   Set_Visibility (Foreign_Naming_Scheme_Editor.Combo_Entry11, True);

   Gtk_New (Foreign_Naming_Scheme_Editor.Frame47, -"Exceptions");
   Set_Shadow_Type (Foreign_Naming_Scheme_Editor.Frame47, Shadow_Etched_In);
   Pack_Start (Foreign_Naming_Scheme_Editor.Main_Box, Foreign_Naming_Scheme_Editor.Frame47, True, True, 0);

   Gtk_New_Vbox (Foreign_Naming_Scheme_Editor.Vbox57, False, 0);
   Set_Border_Width (Foreign_Naming_Scheme_Editor.Vbox57, 3);
   Add (Foreign_Naming_Scheme_Editor.Frame47, Foreign_Naming_Scheme_Editor.Vbox57);

   Gtk_New (Foreign_Naming_Scheme_Editor.Scrolledwindow3);
   Set_Policy (Foreign_Naming_Scheme_Editor.Scrolledwindow3, Policy_Automatic, Policy_Automatic);
   Pack_Start (Foreign_Naming_Scheme_Editor.Vbox57, Foreign_Naming_Scheme_Editor.Scrolledwindow3, True, True, 0);

   Gtk_New (Foreign_Naming_Scheme_Editor.Viewport2);
   Set_Shadow_Type (Foreign_Naming_Scheme_Editor.Viewport2, Shadow_In);
   Add (Foreign_Naming_Scheme_Editor.Scrolledwindow3, Foreign_Naming_Scheme_Editor.Viewport2);

   Gtk_New (Foreign_Naming_Scheme_Editor.Scrolledwindow4);
   Set_Policy (Foreign_Naming_Scheme_Editor.Scrolledwindow4, Policy_Automatic, Policy_Automatic);
   Add (Foreign_Naming_Scheme_Editor.Viewport2, Foreign_Naming_Scheme_Editor.Scrolledwindow4);

   Gtk_New (Foreign_Naming_Scheme_Editor.Exception_List, 1);
   Set_Selection_Mode (Foreign_Naming_Scheme_Editor.Exception_List, Selection_Single);
   Set_Shadow_Type (Foreign_Naming_Scheme_Editor.Exception_List, Shadow_In);
   Set_Show_Titles (Foreign_Naming_Scheme_Editor.Exception_List, True);
   Set_Column_Width (Foreign_Naming_Scheme_Editor.Exception_List, 0, 80);
   Widget_Callback.Object_Connect
     (Foreign_Naming_Scheme_Editor.Exception_List, "select_row", On_Exception_List_Select_Row'Access, Foreign_Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Foreign_Naming_Scheme_Editor.Exception_List, "key_press_event", On_Exception_List_Key_Press_Event'Access, Foreign_Naming_Scheme_Editor);
   Add (Foreign_Naming_Scheme_Editor.Scrolledwindow4, Foreign_Naming_Scheme_Editor.Exception_List);

   Gtk_New (Foreign_Naming_Scheme_Editor.Label77, -("filename"));
   Set_Alignment (Foreign_Naming_Scheme_Editor.Label77, 0.5, 0.5);
   Set_Padding (Foreign_Naming_Scheme_Editor.Label77, 0, 0);
   Set_Justify (Foreign_Naming_Scheme_Editor.Label77, Justify_Center);
   Set_Line_Wrap (Foreign_Naming_Scheme_Editor.Label77, False);
   Set_Column_Widget (Foreign_Naming_Scheme_Editor.Exception_List, 0, Foreign_Naming_Scheme_Editor.Label77);

   Gtk_New_Hbox (Foreign_Naming_Scheme_Editor.Hbox10, False, 3);
   Pack_Start (Foreign_Naming_Scheme_Editor.Vbox57, Foreign_Naming_Scheme_Editor.Hbox10, False, False, 0);

   Gtk_New (Foreign_Naming_Scheme_Editor.Filename_Entry);
   Set_Editable (Foreign_Naming_Scheme_Editor.Filename_Entry, True);
   Set_Max_Length (Foreign_Naming_Scheme_Editor.Filename_Entry, 0);
   Set_Text (Foreign_Naming_Scheme_Editor.Filename_Entry, -"");
   Set_Visibility (Foreign_Naming_Scheme_Editor.Filename_Entry, True);
   Pack_Start (Foreign_Naming_Scheme_Editor.Hbox10, Foreign_Naming_Scheme_Editor.Filename_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Foreign_Naming_Scheme_Editor.Filename_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Foreign_Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Foreign_Naming_Scheme_Editor.Filename_Entry, "key_press_event", On_Filename_Entry_Key_Press_Event'Access, Foreign_Naming_Scheme_Editor);

   Gtk_New (Foreign_Naming_Scheme_Editor.Update, -"Update");
   Pack_Start (Foreign_Naming_Scheme_Editor.Hbox10, Foreign_Naming_Scheme_Editor.Update, False, False, 0);
   Widget_Callback.Object_Connect
     (Foreign_Naming_Scheme_Editor.Update, "clicked",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Foreign_Naming_Scheme_Editor);

end Initialize;

end Foreign_Naming_Scheme_Editor_Pkg;
