------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with GNAT.Strings;
with Gtk;                       use Gtk;
with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Widget;                use Gtk.Widget;

with GPS.Intl;                  use GPS.Intl;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;
with Gtkada.Handlers;           use Gtkada.Handlers;

with GUI_Utils;                 use GUI_Utils;

package body Breakpoints_Pkg is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Breakpoints : out Breakpoints_Access) is
   begin
      Breakpoints := new Breakpoints_Record;
      Breakpoints_Pkg.Initialize (Breakpoints);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Breakpoints : access Breakpoints_Record'Class) is
      pragma Suppress (All_Checks);
      Bp_Kind_Group  : Widget_SList.GSlist;
      Bp_Kind_Vbox   : Gtk_Vbox;
      Line_Spin_Adj  : Gtk_Adjustment;
      Vbox9_Group    : Widget_SList.GSlist;
      Alignment      : Gtk_Alignment;
      HBox           : Gtk_Hbox;
      Label          : Gtk_Label;
      Main_Vbox      : Gtk_Box;
      Scroll         : Gtk_Scrolled_Window;

   begin
      Gtk.Box.Initialize_Vbox (Breakpoints, False, 0);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Pack_Start (Breakpoints, Scroll);
      Gtk_New_Vbox (Main_Vbox);
      Add_With_Viewport (Scroll, Main_Vbox);

      Gtk_New (Breakpoints.Notebook1);
      Set_Scrollable (Breakpoints.Notebook1, False);
      Set_Show_Border (Breakpoints.Notebook1, True);
      Set_Show_Tabs (Breakpoints.Notebook1, True);
      Set_Tab_Pos (Breakpoints.Notebook1, Pos_Top);
      Pack_Start (Main_Vbox, Breakpoints.Notebook1, True, True, 0);

      Gtk_New_Hbox (Breakpoints.Hbox2, False, 0);
      Add (Breakpoints.Notebook1, Breakpoints.Hbox2);

      Gtk_New_Vbox (Breakpoints.Vbox2, False, 0);
      Set_Border_Width (Breakpoints.Vbox2, 7);
      Pack_Start (Breakpoints.Hbox2, Breakpoints.Vbox2, True, True, 0);

      Gtk_New (Breakpoints.Frame12);
      Set_Shadow_Type (Breakpoints.Frame12, Shadow_Etched_In);
      Pack_Start (Breakpoints.Vbox2, Breakpoints.Frame12, True, True, 0);

      Gtk_New_Vbox (Bp_Kind_Vbox, False, 0);
      Set_Border_Width (Bp_Kind_Vbox, 3);
      Add (Breakpoints.Frame12, Bp_Kind_Vbox);

      --  Source location

      Gtk_New (Breakpoints.Location_Selected, Label => -"Source location");
      Bp_Kind_Group := Get_Group (Breakpoints.Location_Selected);
      Set_Active (Breakpoints.Location_Selected, False);
      Pack_Start
        (Bp_Kind_Vbox, Breakpoints.Location_Selected, False, False, 0);
      Widget_Callback.Object_Connect
        (Breakpoints.Location_Selected, Signal_Toggled,
         Widget_Callback.To_Marshaller (On_Location_Selected_Toggled'Access),
         Breakpoints);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Pack_Start (Bp_Kind_Vbox, Alignment, False, False, 1);
      Gtk_New_Hbox (HBox, False, 0);
      Add (Alignment, HBox);

      Gtk_New (Label, -("File:"));
      Set_Alignment (Label, 0.5, 0.5);
      Set_Padding (Label, 5, 0);
      Set_Justify (Label, Justify_Center);
      Set_Line_Wrap (Label, False);
      Pack_Start (HBox, Label, False, False, 0);

      Gtk_New (Breakpoints.File_Name);
      Set_Editable (Breakpoints.File_Name, True);
      Set_Max_Length (Breakpoints.File_Name, 0);
      Set_Text (Breakpoints.File_Name, "");
      Set_Visibility (Breakpoints.File_Name, True);
      Pack_Start (HBox, Breakpoints.File_Name, True, True, 0);

      Gtk_New (Label, -("Line:"));
      Set_Alignment (Label, 1.0, 0.5);
      Set_Padding (Label, 8, 0);
      Set_Justify (Label, Justify_Center);
      Set_Line_Wrap (Label, False);
      Pack_Start (HBox, Label, False, False, 0);

      Gtk_New (Line_Spin_Adj, 1.0, 1.0, 1.0e+08, 1.0, 10.0);
      Gtk_New (Breakpoints.Line_Spin, Line_Spin_Adj, 1.0, 0);
      Set_Numeric (Breakpoints.Line_Spin, False);
      Set_Snap_To_Ticks (Breakpoints.Line_Spin, False);
      Set_Update_Policy (Breakpoints.Line_Spin, Update_Always);
      Set_Value (Breakpoints.Line_Spin, 1.0);
      Set_Wrap (Breakpoints.Line_Spin, False);
      Pack_Start (HBox, Breakpoints.Line_Spin, True, True, 0);

      --  Subprogram name

      Gtk_New
        (Breakpoints.Subprogram_Selected, Bp_Kind_Group, -"Subprogram Name");
      Bp_Kind_Group := Get_Group (Breakpoints.Subprogram_Selected);
      Set_Active (Breakpoints.Subprogram_Selected, False);
      Pack_Start
        (Bp_Kind_Vbox, Breakpoints.Subprogram_Selected, False, False, 0);
      Widget_Callback.Object_Connect
        (Breakpoints.Subprogram_Selected, Signal_Toggled,
         Widget_Callback.To_Marshaller (On_Subprogam_Selected_Toggled'Access),
         Breakpoints);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Pack_Start (Bp_Kind_Vbox, Alignment, False, False, 1);

      Gtk_New_With_Entry (Breakpoints.Subprogram_Combo);
      Breakpoints.Subprogram_Combo.Append_Text ("");
      Set_Sensitive (Breakpoints.Subprogram_Combo, False);
      Add (Alignment, Breakpoints.Subprogram_Combo);

      --  Address

      Gtk_New (Breakpoints.Address_Selected, Bp_Kind_Group, -"Address");
      Bp_Kind_Group := Get_Group (Breakpoints.Address_Selected);
      Set_Active (Breakpoints.Address_Selected, False);
      Pack_Start (Bp_Kind_Vbox, Breakpoints.Address_Selected, False, False, 0);
      Widget_Callback.Object_Connect
        (Breakpoints.Address_Selected, Signal_Toggled,
         Widget_Callback.To_Marshaller
           (On_Address_Selected_Toggled'Access), Breakpoints);

      Gtk_New
        (Alignment, 0.5, 0.5, 0.88, 0.88);
      Pack_Start (Bp_Kind_Vbox, Alignment, False, False, 1);

      Gtk_New_With_Entry (Breakpoints.Address_Combo);
      Breakpoints.Address_Combo.Append_Text ("");
      Set_Sensitive (Breakpoints.Address_Combo, False);
      Add (Alignment, Breakpoints.Address_Combo);

      --  Regular expressions

      Gtk_New
        (Breakpoints.Regexp_Selected, Bp_Kind_Group, -"Regular expression");
      Set_Active (Breakpoints.Regexp_Selected, False);
      Pack_Start (Bp_Kind_Vbox, Breakpoints.Regexp_Selected, False, False, 0);
      Widget_Callback.Object_Connect
        (Breakpoints.Regexp_Selected, Signal_Toggled,
         Widget_Callback.To_Marshaller (On_Regexp_Selected_Toggled'Access),
         Breakpoints);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Pack_Start (Bp_Kind_Vbox, Alignment, False, False, 1);

      Gtk_New_With_Entry (Breakpoints.Regexp_Combo);
      Breakpoints.Regexp_Combo.Append_Text ("");
      Set_Sensitive (Breakpoints.Regexp_Combo, False);
      Add (Alignment, Breakpoints.Regexp_Combo);

      Gtk_New (Breakpoints.Temporary_Location, -"Temporary breakpoint");
      Set_Active (Breakpoints.Temporary_Location, False);
      Pack_Start (Breakpoints.Vbox2, Breakpoints.Temporary_Location,
                  False, False, 5);

      Gtk_New_Vseparator (Breakpoints.Vseparator1);
      Pack_Start (Breakpoints.Hbox2, Breakpoints.Vseparator1, False, False, 0);

      Gtk_New (Breakpoints.Vbuttonbox2);
      Set_Spacing (Breakpoints.Vbuttonbox2, 10);
      Set_Layout (Breakpoints.Vbuttonbox2, Buttonbox_Start);
      Pack_Start (Breakpoints.Hbox2, Breakpoints.Vbuttonbox2, False, False, 0);

      Gtk_New_From_Stock (Breakpoints.Add_Location, Stock_Add);
      Add (Breakpoints.Vbuttonbox2, Breakpoints.Add_Location);

      Gtk_New (Breakpoints.Location, -("Location"));
      Set_Alignment (Breakpoints.Location, 0.5, 0.5);
      Set_Padding (Breakpoints.Location, 0, 0);
      Set_Justify (Breakpoints.Location, Justify_Center);
      Set_Line_Wrap (Breakpoints.Location, False);
      Set_Tab_Label
        (Breakpoints.Notebook1,
         Get_Nth_Page (Breakpoints.Notebook1, 0),
         Breakpoints.Location);

      Gtk_New_Hbox (Breakpoints.Hbox3, False, 0);
      Add (Breakpoints.Notebook1, Breakpoints.Hbox3);

      Gtk_New_Vbox (Breakpoints.Vbox7, False, 0);
      Set_Border_Width (Breakpoints.Vbox7, 7);
      Pack_Start (Breakpoints.Hbox3, Breakpoints.Vbox7, True, True, 0);

      Gtk_New (Breakpoints.Label9, -("Break when the variable:"));
      Set_Alignment (Breakpoints.Label9, 0.0, 0.5);
      Set_Padding (Breakpoints.Label9, 0, 0);
      Set_Justify (Breakpoints.Label9, Justify_Left);
      Set_Line_Wrap (Breakpoints.Label9, False);
      Pack_Start (Breakpoints.Vbox7, Breakpoints.Label9, False, False, 5);

      Gtk_New (Breakpoints.Watchpoint_Name);
      Set_Editable (Breakpoints.Watchpoint_Name, True);
      Set_Max_Length (Breakpoints.Watchpoint_Name, 0);
      Set_Text (Breakpoints.Watchpoint_Name, -"");
      Set_Visibility (Breakpoints.Watchpoint_Name, True);
      Pack_Start (Breakpoints.Vbox7, Breakpoints.Watchpoint_Name,
                  False, False, 0);

      Gtk_New (Breakpoints.Label10, -("is"));
      Set_Alignment (Breakpoints.Label10, 0.0, 0.5);
      Set_Padding (Breakpoints.Label10, 0, 0);
      Set_Justify (Breakpoints.Label10, Justify_Left);
      Set_Line_Wrap (Breakpoints.Label10, False);
      Pack_Start (Breakpoints.Vbox7, Breakpoints.Label10, False, False, 5);

      Gtk_New (Breakpoints.Watchpoint_Type);
      Breakpoints.Watchpoint_Type.Append_Text (-"written");
      Breakpoints.Watchpoint_Type.Append_Text (-"read");
      Breakpoints.Watchpoint_Type.Append_Text (-"read or written");
      Breakpoints.Watchpoint_Type.Set_Active (1); --  "read"
      Pack_Start
        (Breakpoints.Vbox7, Breakpoints.Watchpoint_Type, False, False, 0);

      Gtk_New (Breakpoints.Label12, -("Condition:"));
      Set_Alignment (Breakpoints.Label12, 0.0, 0.5);
      Set_Padding (Breakpoints.Label12, 0, 0);
      Set_Justify (Breakpoints.Label12, Justify_Left);
      Set_Line_Wrap (Breakpoints.Label12, False);
      Pack_Start (Breakpoints.Vbox7, Breakpoints.Label12, False, False, 5);

      Gtk_New (Breakpoints.Watchpoint_Cond);
      Set_Editable (Breakpoints.Watchpoint_Cond, True);
      Set_Max_Length (Breakpoints.Watchpoint_Cond, 0);
      Set_Text (Breakpoints.Watchpoint_Cond, -"");
      Set_Visibility (Breakpoints.Watchpoint_Cond, True);
      Pack_Start
        (Breakpoints.Vbox7, Breakpoints.Watchpoint_Cond, False, False, 0);

      Gtk_New_Vseparator (Breakpoints.Vseparator2);
      Pack_Start (Breakpoints.Hbox3, Breakpoints.Vseparator2, False, False, 0);

      Gtk_New (Breakpoints.Vbuttonbox3);
      Set_Spacing (Breakpoints.Vbuttonbox3, 10);
      Set_Layout (Breakpoints.Vbuttonbox3, Buttonbox_Start);
      Pack_Start (Breakpoints.Hbox3, Breakpoints.Vbuttonbox3, False, True, 0);

      Gtk_New_From_Stock (Breakpoints.Add_Watchpoint, Stock_Add);
      Add (Breakpoints.Vbuttonbox3, Breakpoints.Add_Watchpoint);

      Gtk_New (Breakpoints.Watchpoint, -("Variable"));
      Set_Alignment (Breakpoints.Watchpoint, 0.5, 0.5);
      Set_Padding (Breakpoints.Watchpoint, 0, 0);
      Set_Justify (Breakpoints.Watchpoint, Justify_Center);
      Set_Line_Wrap (Breakpoints.Watchpoint, False);
      Set_Tab_Label
        (Breakpoints.Notebook1,
         Get_Nth_Page (Breakpoints.Notebook1, 1),
         Breakpoints.Watchpoint);

      Gtk_New_Hbox (Breakpoints.Hbox4, False, 0);
      Add (Breakpoints.Notebook1, Breakpoints.Hbox4);
      Set_Name (Breakpoints.Hbox4, "Breakpoints.Hbox4"); -- For testsuite

      Gtk_New_Vbox (Breakpoints.Vbox8, False, 0);
      Set_Border_Width (Breakpoints.Vbox8, 7);
      Pack_Start (Breakpoints.Hbox4, Breakpoints.Vbox8, True, True, 0);

      Gtk_New (Breakpoints.Label11, -("Break on exception:"));
      Set_Alignment (Breakpoints.Label11, 0.0, 0.5);
      Set_Padding (Breakpoints.Label11, 0, 0);
      Set_Justify (Breakpoints.Label11, Justify_Left);
      Set_Line_Wrap (Breakpoints.Label11, False);
      Pack_Start (Breakpoints.Vbox8, Breakpoints.Label11, False, False, 0);

      Gtk_New_Hbox (Breakpoints.Hbox14, False, 8);
      Pack_Start (Breakpoints.Vbox8, Breakpoints.Hbox14, False, True, 0);

      Gtk_New_With_Entry (Breakpoints.Exception_Name);
      Breakpoints.Exception_Name.Append_Text ("All Ada exceptions");
      Breakpoints.Exception_Name.Set_Active (0);
      Pack_Start
        (Breakpoints.Hbox14, Breakpoints.Exception_Name, True, True, 0);

      Gtk_New (Breakpoints.Load_Exception_List, -"Load List");
      Set_Relief (Breakpoints.Load_Exception_List, Relief_Normal);
      Pack_Start (Breakpoints.Hbox14,
                  Breakpoints.Load_Exception_List, False, False, 0);

      Gtk_New (Breakpoints.Temporary_Exception, -"Temporary breakpoint");
      Set_Active (Breakpoints.Temporary_Exception, False);
      Pack_Start (Breakpoints.Vbox8, Breakpoints.Temporary_Exception,
                  False, False, 0);

      Gtk_New (Breakpoints.Frame4, -"Action");
      Set_Shadow_Type (Breakpoints.Frame4, Shadow_Etched_In);
      Pack_Start (Breakpoints.Vbox8, Breakpoints.Frame4, False, False, 7);

      Gtk_New_Vbox (Breakpoints.Vbox9, False, 0);
      Set_Border_Width (Breakpoints.Vbox9, 7);
      Add (Breakpoints.Frame4, Breakpoints.Vbox9);

      Gtk_New (Breakpoints.Stop_Always_Exception, Vbox9_Group, -"Stop always");
      Vbox9_Group := Get_Group (Breakpoints.Stop_Always_Exception);
      Set_Active (Breakpoints.Stop_Always_Exception, True);
      Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Always_Exception,
                  False, False, 0);

      Gtk_New (Breakpoints.Stop_Not_Handled_Exception,
               Vbox9_Group, -"Stop if not handled");
      Vbox9_Group := Get_Group (Breakpoints.Stop_Not_Handled_Exception);
      Set_Active (Breakpoints.Stop_Not_Handled_Exception, False);
      Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Not_Handled_Exception,
                  False, False, 0);

      Gtk_New_Vseparator (Breakpoints.Vseparator3);
      Pack_Start (Breakpoints.Hbox4, Breakpoints.Vseparator3, False, False, 0);

      Gtk_New (Breakpoints.Vbuttonbox4);
      Set_Spacing (Breakpoints.Vbuttonbox4, 10);
      Set_Layout (Breakpoints.Vbuttonbox4, Buttonbox_Start);
      Pack_Start (Breakpoints.Hbox4, Breakpoints.Vbuttonbox4, False, False, 0);

      Gtk_New_From_Stock (Breakpoints.Add_Exception, Stock_Add);
      Add (Breakpoints.Vbuttonbox4, Breakpoints.Add_Exception);

      Gtk_New (Breakpoints.Except, -("Exception"));
      Set_Alignment (Breakpoints.Except, 0.5, 0.5);
      Set_Padding (Breakpoints.Except, 0, 0);
      Set_Justify (Breakpoints.Except, Justify_Center);
      Set_Line_Wrap (Breakpoints.Except, False);
      Set_Tab_Label
        (Breakpoints.Notebook1,
         Get_Nth_Page (Breakpoints.Notebook1, 2),
         Breakpoints.Except);

      Gtk_New (Breakpoints.Frame11, -"Breakpoints");
      Set_Shadow_Type (Breakpoints.Frame11, Shadow_Etched_In);
      Pack_Start (Main_Vbox, Breakpoints.Frame11, True, True, 0);

      Gtk_New_Vbox (Breakpoints.Vbox16, False, 0);
      Add (Breakpoints.Frame11, Breakpoints.Vbox16);

      Gtk_New (Breakpoints.Label72,
               -("Click in the 'Enb' column to change the status"));
      Set_Alignment (Breakpoints.Label72, 0.05, 0.5);
      Set_Padding (Breakpoints.Label72, 0, 0);
      Set_Justify (Breakpoints.Label72, Justify_Center);
      Set_Line_Wrap (Breakpoints.Label72, False);
      Pack_Start (Breakpoints.Vbox16, Breakpoints.Label72, False, False, 0);

      Gtk_New (Breakpoints.Scrolledwindow2);
      Set_Policy
        (Breakpoints.Scrolledwindow2, Policy_Automatic, Policy_Automatic);
      Pack_Start
        (Breakpoints.Vbox16, Breakpoints.Scrolledwindow2, True, True, 0);

      declare
         Column_Types : constant Glib.GType_Array (1 .. 8) :=
           (Guint (Col_Enb + 1)  => GType_Boolean,
            others   => GType_String);

         Column_Names : GNAT.Strings.String_List (1 .. 8) :=
           (new String'(-"Num"),
            new String'(-"Enb"),
            new String'(-"Type"),
            new String'(-"Disp"),
            new String'(-"File/Variable"),
            new String'(-"Line"),
            new String'(-"Exception"),
            new String'(-"Subprograms"));
      begin
         Breakpoints.Breakpoint_List :=
           Create_Tree_View
             (Column_Types, Column_Names, Sortable_Columns => False);

         for J in Column_Names'Range loop
            GNAT.Strings.Free (Column_Names (J));
         end loop;
      end;

      Add (Breakpoints.Scrolledwindow2, Breakpoints.Breakpoint_List);

      Gtk_New (Breakpoints.Hbuttonbox8);
      Set_Spacing (Breakpoints.Hbuttonbox8, 30);
      Set_Layout (Breakpoints.Hbuttonbox8, Buttonbox_Spread);
      Pack_Start
        (Breakpoints.Vbox16, Breakpoints.Hbuttonbox8, False, False, 0);

      Gtk_New_From_Stock (Breakpoints.Remove, Stock_Remove);
      Add (Breakpoints.Hbuttonbox8, Breakpoints.Remove);

      Gtk_New (Breakpoints.View, -"View");
      Set_Relief (Breakpoints.View, Relief_Normal);
      Add (Breakpoints.Hbuttonbox8, Breakpoints.View);

      Gtk_New (Breakpoints.Advanced_Location, -"Advanced");
      Set_Relief (Breakpoints.Advanced_Location, Relief_Normal);
      Add (Breakpoints.Hbuttonbox8, Breakpoints.Advanced_Location);
   end Initialize;

end Breakpoints_Pkg;
