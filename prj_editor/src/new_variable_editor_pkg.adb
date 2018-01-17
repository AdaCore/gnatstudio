------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with Gtk;             use Gtk;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Stock;       use Gtk.Stock;
with GPS.Intl;        use GPS.Intl;
with GPS.Main_Window; use GPS.Main_Window;

package body New_Variable_Editor_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (New_Variable_Editor : out New_Variable_Editor_Access;
      Title               : String;
      Kernel              : not null access Kernel_Handle_Record'Class)
   is
   begin
      New_Variable_Editor := new New_Variable_Editor_Record;
      New_Variable_Editor_Pkg.Initialize (New_Variable_Editor, Title, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (New_Variable_Editor : access New_Variable_Editor_Record'Class;
      Title               : String;
      Kernel              : not null access Kernel_Handle_Record'Class)
   is
   begin
      GPS.Dialogs.Initialize
        (New_Variable_Editor,
         Title => Title,
         Kernel => Kernel);
      Set_Default_Size_From_History
        (New_Variable_Editor, "project-variable", Kernel, 600, 400);

      New_Variable_Editor.Dialog_Vbox1 :=
        Get_Content_Area (New_Variable_Editor);
      Set_Homogeneous (New_Variable_Editor.Dialog_Vbox1, False);
      Set_Spacing (New_Variable_Editor.Dialog_Vbox1, 0);

      New_Variable_Editor.Dialog_Action_Area1 :=
        Get_Action_Area (New_Variable_Editor);
      Set_Border_Width (New_Variable_Editor.Dialog_Action_Area1, 10);
      Set_Homogeneous (New_Variable_Editor.Dialog_Action_Area1, True);
      Set_Spacing (New_Variable_Editor.Dialog_Action_Area1, 5);

      Gtk_New (New_Variable_Editor.Table1, 2, 2, False);
      Set_Border_Width (New_Variable_Editor.Table1, 10);
      Set_Row_Spacings (New_Variable_Editor.Table1, 5);
      Set_Col_Spacings (New_Variable_Editor.Table1, 0);
      Pack_Start (New_Variable_Editor.Dialog_Vbox1,
                  New_Variable_Editor.Table1, True, True, 0);

      Gtk_New (New_Variable_Editor.Label58, -("Name:"));
      Set_Alignment (New_Variable_Editor.Label58, 0.0, 0.5);
      Set_Padding (New_Variable_Editor.Label58, 10, 0);
      Set_Justify (New_Variable_Editor.Label58, Justify_Left);
      Set_Line_Wrap (New_Variable_Editor.Label58, False);
      Attach (New_Variable_Editor.Table1,
              New_Variable_Editor.Label58, 0, 1, 0, 1,
              Fill, Fill,
              0, 0);

      Gtk_New_With_Entry (New_Variable_Editor.Variable_Name);
      New_Variable_Editor.Variable_Name.Append_Text ("");
      Attach (New_Variable_Editor.Table1, New_Variable_Editor.Variable_Name,
              1, 2, 0, 1,
              Expand or Fill, 0,
              0, 0);

      Gtk_New (New_Variable_Editor.Label60, -("Possible values:"));
      Set_Alignment (New_Variable_Editor.Label60, 7.45058e-09, 7.45058e-09);
      Set_Padding (New_Variable_Editor.Label60, 10, 0);
      Set_Justify (New_Variable_Editor.Label60, Justify_Center);
      Set_Line_Wrap (New_Variable_Editor.Label60, False);
      Attach (New_Variable_Editor.Table1, New_Variable_Editor.Label60,
              0, 1, 1, 2,
              Fill, Fill,
              0, 0);

      Gtk_New (New_Variable_Editor.Scrolledwindow2);
      Set_Policy
        (New_Variable_Editor.Scrolledwindow2, Policy_Automatic,
         Policy_Automatic);
      Attach (New_Variable_Editor.Table1, New_Variable_Editor.Scrolledwindow2,
              1, 2, 1, 2,
              Fill, Expand or Shrink or Fill,
              0, 0);

      Gtk_New (New_Variable_Editor.Viewport1);
      Set_Shadow_Type (New_Variable_Editor.Viewport1, Shadow_In);
      Add (New_Variable_Editor.Scrolledwindow2, New_Variable_Editor.Viewport1);

      Gtk_New_Vbox (New_Variable_Editor.Vbox54, False, 0);
      Add (New_Variable_Editor.Viewport1, New_Variable_Editor.Vbox54);

      Gtk_New (New_Variable_Editor.Values_List);
      Pack_Start
        (New_Variable_Editor.Vbox54,
         New_Variable_Editor.Values_List, True, True, 0);

      Gtk_New (New_Variable_Editor.Hbuttonbox4);
      Set_Spacing (New_Variable_Editor.Hbuttonbox4, 30);
      Set_Layout (New_Variable_Editor.Hbuttonbox4, Buttonbox_End);
      Pack_Start (New_Variable_Editor.Vbox54, New_Variable_Editor.Hbuttonbox4,
                  False, False, 0);

      Gtk_New_From_Stock (New_Variable_Editor.Delete_Variable, Stock_Remove);
      Add (New_Variable_Editor.Hbuttonbox4,
           New_Variable_Editor.Delete_Variable);

      Gtk_New_From_Stock (New_Variable_Editor.New_Variable, Stock_Add);
      Add (New_Variable_Editor.Hbuttonbox4, New_Variable_Editor.New_Variable);

      Gtk_New (New_Variable_Editor.Rename_Variable, -"Rename");
      Add (New_Variable_Editor.Hbuttonbox4,
           New_Variable_Editor.Rename_Variable);

   end Initialize;

end New_Variable_Editor_Pkg;
