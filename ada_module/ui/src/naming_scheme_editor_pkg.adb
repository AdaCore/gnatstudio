------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Glib;                   use Glib;

with Gtk;                    use Gtk;
with Gtk.Alignment;          use Gtk.Alignment;
with Gtk.Combo_Box;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Handlers;        use Gtkada.Handlers;

with GPS.Intl;               use GPS.Intl;
with GUI_Utils;              use GUI_Utils;

with Naming_Scheme_Editor_Pkg.Callbacks;
use Naming_Scheme_Editor_Pkg.Callbacks;

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
      Frame28                  : Gtk_Frame;
      Frame30                  : Gtk_Frame;
      Scrolledwindow1          : Gtk_Scrolled_Window;
      Render                   : Gtk_Cell_Renderer_Text;
      Col                      : Gtk_Tree_View_Column;
      Col_Number               : Gint;
      pragma Unreferenced (Col_Number);

   begin
      Gtk.Window.Initialize (Naming_Scheme_Editor, Window_Toplevel);
      Set_Title (Naming_Scheme_Editor, -"Naming scheme");
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
      Naming_Scheme_Editor.Standard_Scheme.Append_Text (-"GNAT default");
      Naming_Scheme_Editor.Standard_Scheme.Append_Text
        (-"unit.separate.1.ada");
      Naming_Scheme_Editor.Standard_Scheme.Append_Text
        (-"unit__separate_.ada");
      Naming_Scheme_Editor.Standard_Scheme.Append_Text (-"<custom>");
      Naming_Scheme_Editor.Standard_Scheme.Set_Active (0);
      Pack_Start (Hbox4, Naming_Scheme_Editor.Standard_Scheme, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Standard_Scheme, Gtk.Combo_Box.Signal_Changed,
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
      Naming_Scheme_Editor.Casing.Append_Text (-"");
      Naming_Scheme_Editor.Casing.Set_Active (0);
      Pack_Start (Hbox5, Naming_Scheme_Editor.Casing, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Casing, Gtk.Combo_Box.Signal_Changed,
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
        (Naming_Scheme_Editor.Dot_Replacement, Gtk.Combo_Box.Signal_Changed,
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

      Gtk_New_With_Entry (Naming_Scheme_Editor.Spec_Extension);
      Naming_Scheme_Editor.Spec_Extension.Set_Entry_Text_Column (0);
      Naming_Scheme_Editor.Spec_Extension.Append_Text (".ads");
      Naming_Scheme_Editor.Spec_Extension.Append_Text (".1.ada");
      Naming_Scheme_Editor.Spec_Extension.Append_Text ("_.ada");
      Naming_Scheme_Editor.Spec_Extension.Set_Active (0);
      Pack_Start (Hbox6, Naming_Scheme_Editor.Spec_Extension, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Spec_Extension, Gtk.Combo_Box.Signal_Changed,
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

      Gtk_New_With_Entry (Naming_Scheme_Editor.Body_Extension);
      Naming_Scheme_Editor.Body_Extension.Set_Entry_Text_Column (0);
      Naming_Scheme_Editor.Body_Extension.Append_Text (".adb");
      Naming_Scheme_Editor.Body_Extension.Append_Text (".2.ada");
      Naming_Scheme_Editor.Body_Extension.Append_Text (".ada");
      Naming_Scheme_Editor.Body_Extension.Set_Active (0);
      Pack_Start (Hbox8, Naming_Scheme_Editor.Body_Extension, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Body_Extension, Gtk.Combo_Box.Signal_Changed,
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

      Gtk_New_With_Entry (Naming_Scheme_Editor.Separate_Extension);
      Naming_Scheme_Editor.Separate_Extension.Set_Entry_Text_Column (0);
      Naming_Scheme_Editor.Separate_Extension.Append_Text (".adb");
      Naming_Scheme_Editor.Separate_Extension.Append_Text (".2.ada");
      Naming_Scheme_Editor.Separate_Extension.Append_Text (".ada");
      Naming_Scheme_Editor.Separate_Extension.Set_Active (0);
      Pack_Start
        (Hbox9, Naming_Scheme_Editor.Separate_Extension, True, True, 0);
      Widget_Callback.Object_Connect
        (Naming_Scheme_Editor.Separate_Extension, Gtk.Combo_Box.Signal_Changed,
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
        (Get_Selection (Naming_Scheme_Editor.Exception_List),
         Gtk.Tree_Selection.Signal_Changed,
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
