-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk;                    use Gtk;
with Gtk.Alignment;          use Gtk.Alignment;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Enums;              use Gtk.Enums;
with Gtkada.Handlers;        use Gtkada.Handlers;
with GPS.Intl;               use GPS.Intl;
with Naming_Scheme_Editor_Pkg.Callbacks;
use Naming_Scheme_Editor_Pkg.Callbacks;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Glib;                   use Glib;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with GUI_Utils;              use GUI_Utils;
with Gtk.Widget;             use Gtk.Widget;

package body Naming_Scheme_Editor_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Naming_Scheme_Editor : out Naming_Scheme_Editor_Access) is
   begin
      Naming_Scheme_Editor := new Naming_Scheme_Editor_Record;
      Naming_Scheme_Editor_Pkg.Initialize (Naming_Scheme_Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Naming_Scheme_Editor : access Naming_Scheme_Editor_Record'Class)
   is
      pragma Suppress (All_Checks);

      Alignment1               : Gtk_Alignment;
      Hbox3                    : Gtk_Hbox;
      Hbox4                    : Gtk_Hbox;
      Hbox5                    : Gtk_Hbox;
      Hbox6                    : Gtk_Hbox;
      Hbox7                    : Gtk_Hbox;
      Hbox8                    : Gtk_Hbox;
      Hbox9                    : Gtk_Hbox;
      Vbox29                   : Gtk_Vbox;
      Vbox53                   : Gtk_Vbox;
      Combo_Entry2             : Gtk_Entry;
      Combo_Entry3             : Gtk_Entry;
      Combo_Entry4             : Gtk_Entry;
      Combo_Entry5             : Gtk_Entry;
      Combo_Entry6             : Gtk_Entry;
      Frame28                  : Gtk_Frame;
      Frame30                  : Gtk_Frame;
      Scrolledwindow1          : Gtk_Scrolled_Window;
      Standard_Scheme_Items    : String_List.Glist;
      Casing_Items             : String_List.Glist;
      Spec_Extension_Items     : String_List.Glist;
      Body_Extension_Items     : String_List.Glist;
      Separate_Extension_Items : String_List.Glist;
      Render                   : Gtk_Cell_Renderer_Text;
      Col                      : Gtk_Tree_View_Column;
      Col_Number               : Gint;
      pragma Unreferenced (Col_Number);

   begin
      Gtk.Window.Initialize (Naming_Scheme_Editor, Window_Toplevel);
      Set_Title (Naming_Scheme_Editor, -"Naming scheme");
      Set_Policy (Naming_Scheme_Editor, False, True, True);
      Set_Position (Naming_Scheme_Editor, Win_Pos_None);
      Set_Modal (Naming_Scheme_Editor, False);

      Gtk_New (Alignment1, 0.5, 0.5, 1.0, 1.0);
      Add (Naming_Scheme_Editor, Alignment1);

      Gtk_New_Vbox (Naming_Scheme_Editor.Main_Box, False, 9);
      Set_Border_Width (Naming_Scheme_Editor.Main_Box, 4);
      Add (Alignment1, Naming_Scheme_Editor.Main_Box);

      Gtk_New_Hbox (Hbox4, False, 0);
      Pack_Start (Naming_Scheme_Editor.Main_Box, Hbox4, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Label_Naming_Scheme, -("Naming scheme:"));
      Set_Alignment (Naming_Scheme_Editor.Label_Naming_Scheme, 0.0, 0.5);
      Set_Padding (Naming_Scheme_Editor.Label_Naming_Scheme, 10, 0);
      Set_Justify (Naming_Scheme_Editor.Label_Naming_Scheme, Justify_Center);
      Set_Line_Wrap (Naming_Scheme_Editor.Label_Naming_Scheme, False);
      Pack_Start
        (Hbox4, Naming_Scheme_Editor.Label_Naming_Scheme, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Standard_Scheme);
      Set_Case_Sensitive (Naming_Scheme_Editor.Standard_Scheme, False);
      Set_Use_Arrows (Naming_Scheme_Editor.Standard_Scheme, True);
      Set_Use_Arrows_Always (Naming_Scheme_Editor.Standard_Scheme, False);
      String_List.Append (Standard_Scheme_Items, -"GNAT default");
      String_List.Append (Standard_Scheme_Items, -"unit.separate.1.ada");
      String_List.Append (Standard_Scheme_Items, -"unit__separate_.ada");
      String_List.Append (Standard_Scheme_Items, -"<custom>");
      Combo.Set_Popdown_Strings
        (Naming_Scheme_Editor.Standard_Scheme, Standard_Scheme_Items);
      Free_String_List (Standard_Scheme_Items);
      Pack_Start (Hbox4, Naming_Scheme_Editor.Standard_Scheme, True, True, 0);

      Combo_Entry3 := Get_Entry (Naming_Scheme_Editor.Standard_Scheme);
      Set_Editable (Combo_Entry3, False);
      Set_Max_Length (Combo_Entry3, 0);
      Set_Text (Combo_Entry3, -"GNAT default");
      Set_Visibility (Combo_Entry3, True);
      Widget_Callback.Object_Connect
        (Combo_Entry3, Signal_Changed,
         On_Standard_Scheme_Changed'Access, Naming_Scheme_Editor);

      Gtk_New (Frame28, -"Details");
      Set_Shadow_Type (Frame28, Shadow_Etched_In);
      Pack_Start (Naming_Scheme_Editor.Main_Box, Frame28, False, False, 0);

      Gtk_New_Vbox (Vbox53, False, 0);
      Add (Frame28, Vbox53);

      Gtk_New_Hbox (Hbox5, False, 0);
      Pack_Start (Vbox53, Hbox5, True, True, 0);

      Gtk_New (Naming_Scheme_Editor.Label_Casing, -("Filename casing:"));
      Set_Alignment (Naming_Scheme_Editor.Label_Casing, 0.0, 0.5);
      Set_Padding (Naming_Scheme_Editor.Label_Casing, 10, 0);
      Set_Justify (Naming_Scheme_Editor.Label_Casing, Justify_Center);
      Set_Line_Wrap (Naming_Scheme_Editor.Label_Casing, False);
      Pack_Start (Hbox5, Naming_Scheme_Editor.Label_Casing, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Casing);
      Set_Case_Sensitive (Naming_Scheme_Editor.Casing, False);
      Set_Use_Arrows (Naming_Scheme_Editor.Casing, True);
      Set_Use_Arrows_Always (Naming_Scheme_Editor.Casing, False);
      String_List.Append (Casing_Items, -"");
      Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Casing, Casing_Items);
      Free_String_List (Casing_Items);
      Pack_Start (Hbox5, Naming_Scheme_Editor.Casing, True, True, 0);

      Combo_Entry2 := Get_Entry (Naming_Scheme_Editor.Casing);
      Set_Editable (Combo_Entry2, False);
      Set_Max_Length (Combo_Entry2, 0);
      Set_Text (Combo_Entry2, -"");
      Set_Visibility (Combo_Entry2, True);
      Widget_Callback.Object_Connect
        (Combo_Entry2, Signal_Changed,
         Customized'Access, Naming_Scheme_Editor);

      Gtk_New_Hbox (Hbox7, False, 0);
      Pack_Start (Vbox53, Hbox7, True, True, 0);

      Gtk_New
        (Naming_Scheme_Editor.Label_Dot_Replacement, -"Dot replacement:");
      Set_Alignment (Naming_Scheme_Editor.Label_Dot_Replacement, 0.0, 0.5);
      Set_Padding (Naming_Scheme_Editor.Label_Dot_Replacement, 10, 0);
      Set_Justify (Naming_Scheme_Editor.Label_Dot_Replacement, Justify_Center);
      Set_Line_Wrap (Naming_Scheme_Editor.Label_Dot_Replacement, False);
      Pack_Start
        (Hbox7, Naming_Scheme_Editor.Label_Dot_Replacement, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Dot_Replacement);
      Set_Editable (Naming_Scheme_Editor.Dot_Replacement, True);
      Set_Max_Length (Naming_Scheme_Editor.Dot_Replacement, 0);
      Set_Text (Naming_Scheme_Editor.Dot_Replacement, -"-");
      Set_Visibility (Naming_Scheme_Editor.Dot_Replacement, True);
      Pack_Start (Hbox7, Naming_Scheme_Editor.Dot_Replacement, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Dot_Replacement, Signal_Changed,
         Customized'Access, Naming_Scheme_Editor);

      Gtk_New_Hbox (Hbox6, False, 0);
      Pack_Start (Vbox53, Hbox6, True, True, 0);

      Gtk_New
        (Naming_Scheme_Editor.Label_Spec_Extensions, -"Spec extensions:");
      Set_Alignment (Naming_Scheme_Editor.Label_Spec_Extensions, 0.0, 0.5);
      Set_Padding (Naming_Scheme_Editor.Label_Spec_Extensions, 10, 0);
      Set_Justify (Naming_Scheme_Editor.Label_Spec_Extensions, Justify_Center);
      Set_Line_Wrap (Naming_Scheme_Editor.Label_Spec_Extensions, False);
      Pack_Start
        (Hbox6, Naming_Scheme_Editor.Label_Spec_Extensions, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Spec_Extension);
      Set_Case_Sensitive (Naming_Scheme_Editor.Spec_Extension, False);
      Set_Use_Arrows (Naming_Scheme_Editor.Spec_Extension, True);
      Set_Use_Arrows_Always (Naming_Scheme_Editor.Spec_Extension, False);
      String_List.Append (Spec_Extension_Items, -".ads");
      String_List.Append (Spec_Extension_Items, -".1.ada");
      String_List.Append (Spec_Extension_Items, -"_.ada");
      Combo.Set_Popdown_Strings
        (Naming_Scheme_Editor.Spec_Extension, Spec_Extension_Items);
      Free_String_List (Spec_Extension_Items);
      Pack_Start (Hbox6, Naming_Scheme_Editor.Spec_Extension, True, True, 0);

      Combo_Entry4 := Get_Entry (Naming_Scheme_Editor.Spec_Extension);
      Set_Editable (Combo_Entry4, True);
      Set_Max_Length (Combo_Entry4, 0);
      Set_Text (Combo_Entry4, -".ads");
      Set_Visibility (Combo_Entry4, True);
      Widget_Callback.Object_Connect
        (Combo_Entry4, Signal_Changed,
         Customized'Access, Naming_Scheme_Editor);

      Gtk_New_Hbox (Hbox8, False, 0);
      Pack_Start (Vbox53, Hbox8, True, True, 0);

      Gtk_New
        (Naming_Scheme_Editor.Label_Body_Extensions, -("Body extensions:"));
      Set_Alignment (Naming_Scheme_Editor.Label_Body_Extensions, 0.0, 0.5);
      Set_Padding (Naming_Scheme_Editor.Label_Body_Extensions, 10, 0);
      Set_Justify (Naming_Scheme_Editor.Label_Body_Extensions, Justify_Center);
      Set_Line_Wrap (Naming_Scheme_Editor.Label_Body_Extensions, False);
      Pack_Start
        (Hbox8, Naming_Scheme_Editor.Label_Body_Extensions, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Body_Extension);
      Set_Case_Sensitive (Naming_Scheme_Editor.Body_Extension, False);
      Set_Use_Arrows (Naming_Scheme_Editor.Body_Extension, True);
      Set_Use_Arrows_Always (Naming_Scheme_Editor.Body_Extension, False);
      String_List.Append (Body_Extension_Items, -".adb");
      String_List.Append (Body_Extension_Items, -".2.ada");
      String_List.Append (Body_Extension_Items, -".ada");
      Combo.Set_Popdown_Strings
        (Naming_Scheme_Editor.Body_Extension, Body_Extension_Items);
      Free_String_List (Body_Extension_Items);
      Pack_Start (Hbox8, Naming_Scheme_Editor.Body_Extension, True, True, 0);

      Combo_Entry5 := Get_Entry (Naming_Scheme_Editor.Body_Extension);
      Set_Editable (Combo_Entry5, True);
      Set_Max_Length (Combo_Entry5, 0);
      Set_Text (Combo_Entry5, -".adb");
      Set_Visibility (Combo_Entry5, True);
      Widget_Callback.Object_Connect
        (Combo_Entry5, Signal_Changed,
         Customized'Access, Naming_Scheme_Editor);

      Gtk_New_Hbox (Hbox9, False, 0);
      Pack_Start (Vbox53, Hbox9, True, True, 0);

      Gtk_New
        (Naming_Scheme_Editor.Label_Separate_Extensions,
         -"Separate extensions:");
      Set_Alignment (Naming_Scheme_Editor.Label_Separate_Extensions, 0.0, 0.5);
      Set_Padding (Naming_Scheme_Editor.Label_Separate_Extensions, 10, 0);
      Set_Justify
        (Naming_Scheme_Editor.Label_Separate_Extensions, Justify_Center);
      Set_Line_Wrap (Naming_Scheme_Editor.Label_Separate_Extensions, False);
      Pack_Start
        (Hbox9, Naming_Scheme_Editor.Label_Separate_Extensions,
         False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Separate_Extension);
      Set_Case_Sensitive (Naming_Scheme_Editor.Separate_Extension, False);
      Set_Use_Arrows (Naming_Scheme_Editor.Separate_Extension, True);
      Set_Use_Arrows_Always (Naming_Scheme_Editor.Separate_Extension, False);
      String_List.Append (Separate_Extension_Items, -".adb");
      String_List.Append (Separate_Extension_Items, -".2.ada");
      String_List.Append (Separate_Extension_Items, -".ada");
      Combo.Set_Popdown_Strings
        (Naming_Scheme_Editor.Separate_Extension, Separate_Extension_Items);
      Free_String_List (Separate_Extension_Items);
      Pack_Start
        (Hbox9, Naming_Scheme_Editor.Separate_Extension, True, True, 0);

      Combo_Entry6 := Get_Entry (Naming_Scheme_Editor.Separate_Extension);
      Set_Editable (Combo_Entry6, True);
      Set_Max_Length (Combo_Entry6, 0);
      Set_Text (Combo_Entry6, -".adb");
      Set_Visibility (Combo_Entry6, True);
      Widget_Callback.Object_Connect
        (Combo_Entry6, Signal_Changed,
         Customized'Access, Naming_Scheme_Editor);

      Gtk_New (Frame30, -"Exceptions");
      Set_Shadow_Type (Frame30, Shadow_Etched_In);
      Pack_Start (Naming_Scheme_Editor.Main_Box, Frame30, True, True, 0);

      Gtk_New_Vbox (Vbox29, False, 0);
      Set_Border_Width (Vbox29, 3);
      Add (Frame30, Vbox29);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1, Policy_Automatic, Policy_Automatic);
      Pack_Start (Vbox29, Scrolledwindow1, True, True, 0);

      Gtk_New (Naming_Scheme_Editor.Exception_List_Model,
               (0 => GType_String, 1 => GType_String, 2 => GType_String,
                3 => GType_Boolean));
      Gtk_New (Naming_Scheme_Editor.Exception_List,
               Naming_Scheme_Editor.Exception_List_Model);
      Set_Mode
        (Get_Selection (Naming_Scheme_Editor.Exception_List),
         Selection_Single);

      Gtk_New (Col);
      Col_Number := Append_Column (Naming_Scheme_Editor.Exception_List, Col);
      Set_Title (Col, -"Unit name");
      Gtk_New (Render);
      Pack_Start (Col, Render, False);
      Set_Sort_Column_Id (Col, 0);
      Add_Attribute (Col, Render, "text", 0);
      Add_Attribute (Col, Render, "editable", 3);
      Set_Editable_And_Callback
        (Naming_Scheme_Editor.Exception_List_Model, Render, 0);
      Widget_Callback.Object_Connect
        (Render, Signal_Edited, On_Exceptions_List_Select_Row'Access,
         Naming_Scheme_Editor);

      --  Sort on this column by clicking on it
      Clicked (Col);

      Gtk_New (Col);
      Col_Number := Append_Column (Naming_Scheme_Editor.Exception_List, Col);
      Set_Title (Col, -"Spec filename");
      Gtk_New (Render);
      Pack_Start (Col, Render, False);
      Set_Sort_Column_Id (Col, 1);
      Add_Attribute (Col, Render, "text", 1);
      Add_Attribute (Col, Render, "editable", 3);
      Set_Editable_And_Callback
        (Naming_Scheme_Editor.Exception_List_Model, Render, 1);
      Widget_Callback.Object_Connect
        (Render, Signal_Edited, On_Exceptions_List_Select_Row'Access,
         Naming_Scheme_Editor);

      Gtk_New (Col);
      Col_Number := Append_Column (Naming_Scheme_Editor.Exception_List, Col);
      Set_Title (Col, -"Body filename");
      Gtk_New (Render);
      Pack_Start (Col, Render, False);
      Set_Sort_Column_Id (Col, 2);
      Add_Attribute (Col, Render, "text", 2);
      Add_Attribute (Col, Render, "editable", 3);
      Set_Editable_And_Callback
        (Naming_Scheme_Editor.Exception_List_Model, Render, 2);
      Widget_Callback.Object_Connect
        (Render, Signal_Edited, On_Exceptions_List_Select_Row'Access,
         Naming_Scheme_Editor);

      Return_Callback.Object_Connect
        (Naming_Scheme_Editor.Exception_List, Signal_Key_Press_Event,
         On_Exception_List_Key_Press_Event'Access, Naming_Scheme_Editor);
      Widget_Callback.Object_Connect
        (Get_Selection (Naming_Scheme_Editor.Exception_List), Signal_Changed,
         On_Exceptions_List_Select_Row'Access, Naming_Scheme_Editor);
      Add (Scrolledwindow1, Naming_Scheme_Editor.Exception_List);

      Gtk_New_Hbox (Hbox3, False, 3);
      Pack_Start (Vbox29, Hbox3, False, False, 0);

      Gtk_New (Naming_Scheme_Editor.Unit_Name_Entry);
      Set_Editable (Naming_Scheme_Editor.Unit_Name_Entry, True);
      Set_Max_Length (Naming_Scheme_Editor.Unit_Name_Entry, 0);
      Set_Text (Naming_Scheme_Editor.Unit_Name_Entry, -"");
      Set_Visibility (Naming_Scheme_Editor.Unit_Name_Entry, True);
      Pack_Start
        (Hbox3, Naming_Scheme_Editor.Unit_Name_Entry, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Unit_Name_Entry, Gtk.GEntry.Signal_Activate,
         On_Update_Clicked'Access, Naming_Scheme_Editor);
      Return_Callback.Object_Connect
        (Naming_Scheme_Editor.Unit_Name_Entry, Signal_Key_Press_Event,
         On_Unit_Name_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

      Gtk_New (Naming_Scheme_Editor.Spec_Filename_Entry);
      Set_Editable (Naming_Scheme_Editor.Spec_Filename_Entry, True);
      Set_Max_Length (Naming_Scheme_Editor.Spec_Filename_Entry, 0);
      Set_Text (Naming_Scheme_Editor.Spec_Filename_Entry, -"");
      Set_Visibility (Naming_Scheme_Editor.Spec_Filename_Entry, True);
      Pack_Start
        (Hbox3, Naming_Scheme_Editor.Spec_Filename_Entry, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Spec_Filename_Entry, Gtk.GEntry.Signal_Activate,
         On_Update_Clicked'Access, Naming_Scheme_Editor);
      Return_Callback.Object_Connect
        (Naming_Scheme_Editor.Spec_Filename_Entry, Signal_Key_Press_Event,
         On_Spec_Filename_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

      Gtk_New (Naming_Scheme_Editor.Body_Filename_Entry);
      Set_Editable (Naming_Scheme_Editor.Body_Filename_Entry, True);
      Set_Max_Length (Naming_Scheme_Editor.Body_Filename_Entry, 0);
      Set_Text (Naming_Scheme_Editor.Body_Filename_Entry, -"");
      Set_Visibility (Naming_Scheme_Editor.Body_Filename_Entry, True);
      Pack_Start
        (Hbox3, Naming_Scheme_Editor.Body_Filename_Entry, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Body_Filename_Entry, Gtk.GEntry.Signal_Activate,
         On_Update_Clicked'Access, Naming_Scheme_Editor);
      Return_Callback.Object_Connect
        (Naming_Scheme_Editor.Body_Filename_Entry, Signal_Key_Press_Event,
         On_Body_Filename_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

      Gtk_New (Naming_Scheme_Editor.Update, -"Update");
      Pack_Start (Hbox3, Naming_Scheme_Editor.Update, False, False, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Update, Gtk.Button.Signal_Clicked,
         On_Update_Clicked'Access, Naming_Scheme_Editor);
   end Initialize;

end Naming_Scheme_Editor_Pkg;
