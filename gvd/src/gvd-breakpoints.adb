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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Advanced_Breakpoint_Pkg;   use Advanced_Breakpoint_Pkg;
with Debugger;                  use Debugger;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;
with Generic_Views;             use Generic_Views;
with Glib.Object;               use Glib.Object;
with Glib.Values;
with Glib;                      use Glib;
with Glib_Values_Utils;         use Glib_Values_Utils;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Debuggers;             use GPS.Debuggers;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks; use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Spin_Button;           use Gtk.Spin_Button;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Vbutton_Box;           use Gtk.Vbutton_Box;
with Gtk.Widget;                use Gtk.Widget;
with Gtk;                       use Gtk;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with GUI_Utils;                 use GUI_Utils;
with GVD.Code_Editors;          use GVD, GVD.Code_Editors;
with GVD.Generic_View;          use GVD.Generic_View;
with GVD.Process;               use GVD.Process;
with GVD.Types;                 use GVD.Types;
with GVD_Module;                use GVD_Module;
with Process_Proxies;           use Process_Proxies;

package body GVD.Breakpoints is

   Col_Num       : constant Gint := 0;
   Col_Enb       : constant Gint := 1;
   Col_Type      : constant Gint := 2;
   Col_Disp      : constant Gint := 3;
   Col_File      : constant Gint := 4;
   Col_Line      : constant Gint := 5;
   Col_Exception : constant Gint := 6;
   Col_Subprogs  : constant Gint := 7;

   Column_Types : constant Glib.GType_Array (0 .. 7) :=
     (Guint (Col_Enb) => GType_Boolean,
      others          => GType_String);

   type Breakpoint_Editor_Record is new Process_View_Record with
      record
         Breakpoint_List      : Gtk_Tree_View;

         Notebook             : Gtk_Notebook;

         Subprogram_Selected  : Gtk_Radio_Button;
         Address_Selected     : Gtk_Radio_Button;
         Regexp_Selected      : Gtk_Radio_Button;
         Location_Selected    : Gtk_Radio_Button;
         Temporary_Location   : Gtk_Check_Button;
         Temporary_Exception  : Gtk_Check_Button;

         Hbox_Exceptions            : Gtk_Hbox;
         Stop_Always_Exception      : Gtk_Radio_Button;
         Stop_Not_Handled_Exception : Gtk_Radio_Button;

         Exception_Name       : Gtk_Combo_Box_Text;

         Watchpoint_Name      : Gtk_Entry;
         Watchpoint_Type      : Gtk_Combo_Box_Text;
         Watchpoint_Cond      : Gtk_Entry;

         File_Name            : Gtk_Entry;
         Line_Spin            : Gtk_Spin_Button;
         Address_Combo        : Gtk_Combo_Box_Text;
         Subprogram_Combo     : Gtk_Combo_Box_Text;
         Regexp_Combo         : Gtk_Combo_Box_Text;

         Remove               : Gtk_Button;
         View                 : Gtk_Button;
         Advanced_Location    : Gtk_Button;

         Advanced_Breakpoints : Advanced_Breakpoint_Access;
      end record;
   type Breakpoint_Editor is access all Breakpoint_Editor_Record'Class;

   overriding procedure Update
     (View   : not null access Breakpoint_Editor_Record);
   overriding procedure On_Process_Terminated
     (View : not null access Breakpoint_Editor_Record);
   --  See inherited documentation

   function Initialize
     (Self : access Breakpoint_Editor_Record'Class) return Gtk_Widget;
   --  Internal initialization function
   --  Returns the focus child

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Breakpoint_Editor_Record'Class;
   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Breakpoint_Editor_Record'Class := null);
   --  Store or retrieve the view from the process

   package Breakpoints_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Breakpoints",
      View_Name          => -"Breakpoints",
      Formal_View_Record => Breakpoint_Editor_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Default,
      Position           => Position_Automatic,
      Initialize         => Initialize);
   package Simple_Views is new GVD.Generic_View.Simple_Views
     (Views              => Breakpoints_MDI_Views,
      Formal_View_Record => Breakpoint_Editor_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

   type On_Breakpoints_Changed is new Debugger_Hooks_Function
      with null record;
   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class);
   --  Hook for "debugger_breakpoints_changed"

   procedure Update_Breakpoint_List
     (View   : access Breakpoint_Editor_Record'Class);
   --  Update the list of breakpoints in the dialog.
   --  The list is taken from the one stored in the current debugger session.

   function Get_Selection_Index
     (View : access Breakpoint_Editor_Record'Class) return Integer;
   --  Return the index of the currently selected line in the breakpoint
   --  editor. The index is the element in Editor.Process.Breakpoints, or
   --  -1 if there is no selection

   procedure Breakpoint_Row_Selection_Change
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was selected.

   function Breakpoint_Clicked
     (Widget : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   --  Called when receiving a click on the breakpoint tree.

   procedure On_Add_Location_Clicked (Object : access GObject_Record'Class);
   --  Set the breakpoint that is currently described in the location page.
   --  If Current is not -1, then the breakpoint currently displayed at line
   --  Current is updated (first removed if some of the information has
   --  changed).

   procedure On_Subprogram_Selected_Toggled (W : access GObject_Record'Class);
   procedure On_Address_Selected_Toggled    (W : access GObject_Record'Class);
   procedure On_Regexp_Selected_Toggled     (W : access GObject_Record'Class);
   procedure On_Location_Selected_Toggled   (W : access GObject_Record'Class);
   procedure On_Advanced_Location_Clicked   (W : access GObject_Record'Class);
   procedure On_Load_Exception_List_Clicked (W : access GObject_Record'Class);
   procedure On_Add_Exception_Clicked       (W : access GObject_Record'Class);
   procedure On_Remove_Clicked              (W : access GObject_Record'Class);
   procedure On_View_Clicked                (W : access GObject_Record'Class);
   procedure On_Add_Watchpoint_Clicked      (W : access GObject_Record'Class);
   function On_Breakpoints_Key_Press_Event
     (W      : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key) return Boolean;
   --  Callbacks for the various buttons

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data);
   --  Fills the contents of the Advanced dialog with the values contained in
   --  Br.

   procedure Set_Advanced
     (View : access Breakpoint_Editor_Record'Class;
      Br   : Breakpoint_Data);
   --  Set the advanced options for the breakpoint Br, based on the contents
   --  of its advanced breakpoint editor.

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Breakpoint_Editor_Record'Class is
   begin
      return Breakpoint_Editor (Visual_Debugger (Process).Breakpoints_Editor);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Breakpoint_Editor_Record'Class := null)
   is
      use type Generic_Views.Abstract_View_Access;
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant Breakpoint_Editor := Get_View (Process);
   begin
      V.Breakpoints_Editor := Abstract_View_Access (View);

      --  If we are detaching, clear the old view. This can only be done after
      --  the above, since otherwise the action on the GUI will result into
      --  actions on the debugger.

      if Old /= null then
         On_Process_Terminated (Old);
      end if;

   end Set_View;

   ----------------------------
   -- Update_Breakpoint_List --
   ----------------------------

   procedure Update_Breakpoint_List
     (View   : access Breakpoint_Editor_Record'Class)
   is
      Process      : constant Visual_Debugger := Get_Process (View);
      Selection    : constant Integer := Get_Selection_Index (View);
      Selected     : Breakpoint_Identifier := 0;
      Br           : Breakpoint_Data;
      Size         : Gint;
      pragma Unreferenced (Size);
      Model : constant Gtk_Tree_Store :=
        -Get_Model (View.Breakpoint_List);
      Iter          : Gtk_Tree_Iter;
      Selected_Iter : Gtk_Tree_Iter := Null_Iter;

      Values  : Glib.Values.GValue_Array (1 .. 8);
      Columns : Columns_Array (Values'Range);
      Last    : Gint;

   begin
      Clear (Model);

      if Selection /= -1 then
         Selected := Process.Breakpoints (Selection).Num;
      end if;

      if Process.Breakpoints = null
        or else Process.Breakpoints'Length <= 0
      then
         return;
      end if;

      for B in Process.Breakpoints'Range loop
         Br := Process.Breakpoints (B);

         --  Create a new line

         Append (Model, Iter, Null_Iter);

         Columns (1 .. 4) := (Col_Num, Col_Enb, Col_Type, Col_Disp);
         Values  (1 .. 2) :=
           (1 => As_String (Breakpoint_Identifier'Image (Br.Num)),
            2 => As_Boolean (Br.Enabled));
         Last := 4;

         case Br.The_Type is
            when Breakpoint =>
               Glib.Values.Init_Set_String (Values (3), "break");
            when Watchpoint =>
               Glib.Values.Init_Set_String (Values (3), "watch");
         end case;
         Glib.Values.Init_Set_String
           (Values (4), To_Lower (Br.Disposition'Img));

         if Br.Expression /= null then
            Last := Last + 1;
            Columns (Last) := Col_File;
            Glib.Values.Init_Set_String (Values (Last), Br.Expression.all);
         end if;

         if Br.File /= GNATCOLL.VFS.No_File then
            if Last < 5 then
               Last := Last + 1;
               Columns (Last) := Col_File;
               Glib.Values.Init
                 (Values (Last), Column_Types (Guint (Col_File)));
            end if;
            Glib.Values.Set_String (Values (Last), +Base_Name (Br.File));

            Last := Last + 1;
            Columns (Last) := Col_Line;
            Glib.Values.Init_Set_String
              (Values (Last), Integer'Image (Br.Line));
         end if;

         if Br.Except /= null then
            Last := Last + 1;
            Columns (Last) := Col_Exception;
            Glib.Values.Init_Set_String (Values (Last), Br.Except.all);
         end if;

         if Br.Subprogram /= null then
            Last := Last + 1;
            Columns (Last) := Col_Subprogs;
            Glib.Values.Init_Set_String (Values (Last), Br.Subprogram.all);
         end if;

         Set_And_Clear (Model, Iter, Columns (1 .. Last), Values (1 .. Last));

         if Selection /= -1 and then Br.Num = Selected then
            Selected_Iter := Iter;
         end if;
      end loop;

      --  Reselect the same item as before

      if Selected_Iter /= Null_Iter then
         Select_Iter (Get_Selection (View.Breakpoint_List), Selected_Iter);
      end if;
   end Update_Breakpoint_List;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (View   : not null access Breakpoint_Editor_Record)
   is
      V     : constant Visual_Debugger := Visual_Debugger (Get_Process (View));
      Process  : Process_Proxy_Access;
      M        : Gtk_List_Store;
   begin
      if V /= null then
         Process := Get_Process (V.Debugger);
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         return;
      end if;

      Update_Breakpoint_List (View);

      --  Reinitialize the contents of the file name entry
      Set_Text
        (View.File_Name, +Base_Name (Get_Current_File (V.Editor_Text)));
      --  ??? What if the filesystem path is non-UTF8?

      --  Clear the contents of the exceptions combo (its contents is in fact
      --  cached in gdb, so it is fast enough to call "info exceptions" again)
      M := -View.Exception_Name.Get_Model;
      M.Clear;
      Add_Unique_Combo_Entry (View.Exception_Name, -"All Ada exceptions");
      Add_Unique_Combo_Entry (View.Exception_Name, -"Ada assertions");

      --  Reset the Exception page
      Set_Sensitive (View.Hbox_Exceptions, True);
   end Update;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Kernel);
      Process : constant Visual_Debugger := Visual_Debugger (Debugger);
      View : constant Breakpoint_Editor :=
        Breakpoint_Editor (Process.Breakpoints_Editor);
   begin
      if View /= null then
         Update_Breakpoint_List (View);
      end if;
   end Execute;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Breakpoint_Editor_Record)
   is
      Model : constant Gtk_Tree_Store := -Get_Model (View.Breakpoint_List);
   begin
      Clear (Model);
   end On_Process_Terminated;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Breakpoint_Editor_Record'Class) return Gtk_Widget
   is
      Bbox                : Gtk_Vbutton_Box;
      HBbox               : Gtk_Hbutton_Box;
      Button              : Gtk_Button;
      Alignment           : Gtk_Alignment;
      Bp_Kind_Vbox        : Gtk_Vbox;
      Frame               : Gtk_Frame;
      HBox                : Gtk_Hbox;
      Hbox14              : Gtk_Hbox;
      Hbox2               : Gtk_Hbox;
      Hbox3               : Gtk_Hbox;
      Label               : Gtk_Label;
      Main_Vbox           : Gtk_Box;
      Scroll              : Gtk_Scrolled_Window;
      Vbox16              : Gtk_Vbox;
      Vbox2               : Gtk_Vbox;
      Vbox7               : Gtk_Vbox;
      Vbox8               : Gtk_Vbox;
      Vbox9               : Gtk_Vbox;

   begin
      Gtk.Box.Initialize_Hbox (Self);
      Self.On_Key_Press_Event (On_Breakpoints_Key_Press_Event'Access);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scroll);

      Gtk_New_Vbox (Main_Vbox);
      Scroll.Add_With_Viewport (Main_Vbox);

      Gtk_New (Self.Notebook);
      Main_Vbox.Pack_Start (Self.Notebook, True, True, 0);

      Gtk_New_Hbox (Hbox2, False, 0);
      Self.Notebook.Append_Page
        (Hbox2, Tab_Label => Gtk_Label_New (-"Location"));

      Gtk_New_Vbox (Vbox2, False, 0);
      Hbox2.Pack_Start (Vbox2, True, True, 0);

      Gtk_New_Vbox (Bp_Kind_Vbox, False, 0);
      Vbox2.Pack_Start (Bp_Kind_Vbox, True, True, 0);

      --  Source location

      Gtk_New (Self.Location_Selected, Label => -"Source location");
      Self.Location_Selected.Set_Active (False);
      Bp_Kind_Vbox.Pack_Start (Self.Location_Selected, False, False, 0);
      Self.Location_Selected.On_Toggled
        (On_Location_Selected_Toggled'Access, Self);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Bp_Kind_Vbox.Pack_Start (Alignment, False, False, 1);
      Gtk_New_Hbox (HBox, False, 0);
      Alignment.Add (HBox);

      Gtk_New (Label, -("File:"));
      HBox.Pack_Start (Label, False, False, 0);

      Gtk_New (Self.File_Name);
      Self.File_Name.Set_Editable (True);
      HBox.Pack_Start (Self.File_Name, True, True, 0);
      Self.File_Name.On_Activate (On_Add_Location_Clicked'Access, Self);

      Gtk_New (Label, -("Line:"));
      HBox.Pack_Start (Label, False, False, 0);

      Gtk_New (Self.Line_Spin, Min => 1.0, Max => 1.0e+08, Step => 1.0);
      Self.Line_Spin.Set_Value (1.0);
      HBox.Pack_Start (Self.Line_Spin, True, True, 0);

      --  Subprogram name

      Gtk_New (Self.Subprogram_Selected,
               Group => Self.Location_Selected,
               Label => -"Subprogram Name");
      Bp_Kind_Vbox.Pack_Start (Self.Subprogram_Selected, False, False, 0);
      Self.Subprogram_Selected.On_Toggled
        (On_Subprogram_Selected_Toggled'Access, Self);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Pack_Start (Bp_Kind_Vbox, Alignment, False, False, 1);

      Gtk_New_With_Entry (Self.Subprogram_Combo);
      Self.Subprogram_Combo.Append_Text ("");
      Self.Subprogram_Combo.Set_Sensitive (False);
      Alignment.Add (Self.Subprogram_Combo);
      Gtk_Entry (Self.Subprogram_Combo.Get_Child).On_Activate
        (On_Add_Location_Clicked'Access, Self);

      --  Address

      Gtk_New (Self.Address_Selected,
               Group => Self.Location_Selected,
               Label => -"Address");
      Self.Address_Selected.Set_Active (False);
      Bp_Kind_Vbox.Pack_Start (Self.Address_Selected, False, False, 0);
      Self.Address_Selected.On_Toggled
        (On_Address_Selected_Toggled'Access, Self);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Bp_Kind_Vbox.Pack_Start (Alignment, False, False, 1);

      Gtk_New_With_Entry (Self.Address_Combo);
      Self.Address_Combo.Append_Text ("");
      Self.Address_Combo.Set_Sensitive (False);
      Alignment.Add (Self.Address_Combo);
      Gtk_Entry (Self.Address_Combo.Get_Child).On_Activate
        (On_Add_Location_Clicked'Access, Self);

      --  Regular expressions

      Gtk_New (Self.Regexp_Selected,
               Group => Self.Location_Selected,
               Label => -"Regular expression");
      Self.Regexp_Selected.Set_Active (False);
      Bp_Kind_Vbox.Pack_Start (Self.Regexp_Selected, False, False, 0);
      Self.Regexp_Selected.On_Toggled
        (On_Regexp_Selected_Toggled'Access, Self);

      Gtk_New (Alignment, 0.5, 0.5, 0.88, 0.88);
      Bp_Kind_Vbox.Pack_Start (Alignment, False, False, 1);

      Gtk_New_With_Entry (Self.Regexp_Combo);
      Self.Regexp_Combo.Append_Text ("");
      Self.Regexp_Combo.Set_Sensitive (False);
      Alignment.Add (Self.Regexp_Combo);
      Gtk_Entry (Self.Regexp_Combo.Get_Child).On_Activate
        (On_Add_Location_Clicked'Access, Self);

      Gtk_New (Self.Temporary_Location, -"Temporary breakpoint");
      Self.Temporary_Location.Set_Active (False);
      Vbox2.Pack_Start (Self.Temporary_Location, False, False, 5);

      Hbox2.Pack_Start (Gtk_Vseparator_New, False, False, 0);

      Gtk_New (Bbox);
      Bbox.Set_Layout (Buttonbox_Start);
      Hbox2.Pack_Start (Bbox, False, False, 0);

      Gtk_New_From_Stock (Button, Stock_Add);
      Bbox.Add (Button);
      Button.On_Clicked (On_Add_Location_Clicked'Access, Self);

      Gtk_New_Hbox (Hbox3, False, 0);
      Self.Notebook.Append_Page (Hbox3, Gtk_Label_New (-"Variable"));

      Gtk_New_Vbox (Vbox7, False, 0);
      Hbox3.Pack_Start (Vbox7, True, True, 0);

      Gtk_New (Label, -("Break when the variable:"));
      Vbox7.Pack_Start (Label, False, False, 5);

      Gtk_New (Self.Watchpoint_Name);
      Vbox7.Pack_Start (Self.Watchpoint_Name, False, False, 0);

      Gtk_New (Label, -("is"));
      Vbox7.Pack_Start (Label, False, False, 5);

      Gtk_New (Self.Watchpoint_Type);
      Self.Watchpoint_Type.Append_Text (-"written");
      Self.Watchpoint_Type.Append_Text (-"read");
      Self.Watchpoint_Type.Append_Text (-"read or written");
      Self.Watchpoint_Type.Set_Active (1); --  "read"
      Vbox7.Pack_Start (Self.Watchpoint_Type, False, False, 0);

      Gtk_New (Label, -("Condition:"));
      Vbox7.Pack_Start (Label, False, False, 5);

      Gtk_New (Self.Watchpoint_Cond);
      Vbox7.Pack_Start (Self.Watchpoint_Cond, False, False, 0);

      Hbox3.Pack_Start (Gtk_Vseparator_New, False, False, 0);

      Gtk_New (Bbox);
      Bbox.Set_Layout (Buttonbox_Start);
      Hbox3.Pack_Start (Bbox, False, True, 0);

      Gtk_New_From_Stock (Button, Stock_Add);
      Bbox.Add (Button);
      Button.On_Clicked (On_Add_Watchpoint_Clicked'Access, Self);

      Gtk_New_Hbox (Self.Hbox_Exceptions, False, 0);
      Self.Notebook.Append_Page
        (Self.Hbox_Exceptions, Gtk_Label_New (-"Exception"));
      Self.Hbox_Exceptions.Set_Name ("Breakpoints.Hbox4"); -- For testsuite

      Gtk_New_Vbox (Vbox8, False, 0);
      Self.Hbox_Exceptions.Pack_Start (Vbox8, True, True, 0);

      Gtk_New (Label, -("Break on exception:"));
      Vbox8.Pack_Start (Label, False, False, 0);

      Gtk_New_Hbox (Hbox14, False, 8);
      Vbox8.Pack_Start (Hbox14, False, True, 0);

      Gtk_New_With_Entry (Self.Exception_Name);
      Self.Exception_Name.Append_Text ("All Ada exceptions");
      Self.Exception_Name.Set_Active (0);
      Hbox14.Pack_Start (Self.Exception_Name, True, True, 0);

      Gtk_New (Button, -"Load List");
      Hbox14.Pack_Start (Button, False, False, 0);
      Button.On_Clicked (On_Load_Exception_List_Clicked'Access, Self);

      Gtk_New (Self.Temporary_Exception, -"Temporary breakpoint");
      Self.Temporary_Exception.Set_Active (False);
      Vbox8.Pack_Start (Self.Temporary_Exception, False, False, 0);

      Gtk_New (Frame, -"Action");
      Vbox8.Pack_Start (Frame, False, False, 7);

      Gtk_New_Vbox (Vbox9, False, 0);
      Frame.Add (Vbox9);

      Gtk_New (Self.Stop_Always_Exception, Label => -"Stop always");
      Self.Stop_Always_Exception.Set_Active (True);
      Vbox9.Pack_Start (Self.Stop_Always_Exception, False, False, 0);

      Gtk_New
        (Self.Stop_Not_Handled_Exception,
         Group => Self.Stop_Always_Exception,
         Label => -"Stop if not handled");
      Vbox9.Pack_Start (Self.Stop_Not_Handled_Exception, False, False, 0);

      Self.Hbox_Exceptions.Pack_Start (Gtk_Vseparator_New, False, False, 0);

      Gtk_New (Bbox);
      Bbox.Set_Layout (Buttonbox_Start);
      Self.Hbox_Exceptions.Pack_Start (Bbox, False, False, 0);

      Gtk_New_From_Stock (Button, Stock_Add);
      Bbox.Add (Button);
      Button.On_Clicked (On_Add_Exception_Clicked'Access, Self);

      Gtk_New (Frame, -"Breakpoints");
      Main_Vbox.Pack_Start (Frame, True, True, 0);

      Gtk_New_Vbox (Vbox16, False, 0);
      Frame.Add (Vbox16);

      declare
         Column_Types : constant Glib.GType_Array (1 .. 8) :=
           (Guint (Col_Enb + 1)  => GType_Boolean,
            others               => GType_String);

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
         Self.Breakpoint_List := Create_Tree_View
           (Column_Types, Column_Names, Sortable_Columns => False);
         Self.Breakpoint_List.On_Button_Press_Event
           (Breakpoint_Clicked'Access, Self);
         Widget_Callback.Object_Connect
           (Get_Selection (Self.Breakpoint_List),
            Gtk.Tree_Selection.Signal_Changed,
            Breakpoint_Row_Selection_Change'Access,
            Slot_Object => Self,
            After       => True);

         for J in Column_Names'Range loop
            GNAT.Strings.Free (Column_Names (J));
         end loop;
      end;

      Vbox16.Pack_Start (Self.Breakpoint_List, True, True, 0);

      Gtk_New (HBbox);
      HBbox.Set_Layout (Buttonbox_Spread);
      Vbox16.Pack_Start (HBbox, False, False, 0);

      Gtk_New_From_Stock (Self.Remove, Stock_Remove);
      Self.Remove.Set_Sensitive (False);
      HBbox.Add (Self.Remove);
      Self.Remove.On_Clicked (On_Remove_Clicked'Access, Self);

      Gtk_New (Self.View, -"View");
      Self.View.Set_Sensitive (False);
      HBbox.Add (Self.View);
      Self.View.On_Clicked (On_View_Clicked'Access, Self);

      Gtk_New (Self.Advanced_Location, -"Advanced");
      Self.Advanced_Location.Set_Sensitive (False);
      HBbox.Add (Self.Advanced_Location);
      Self.Advanced_Location.On_Clicked
        (On_Advanced_Location_Clicked'Access, Self);

      --  Return in the combo boxes should activate them

      Debugger_Breakpoints_Changed_Hook.Add
         (new On_Breakpoints_Changed, Watch => Self);

      Show_All (Self);
      return Gtk_Widget (Self);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open breakpoints editor",
         Description => -"Open the Breakpoints Editor for the debugger");
   end Register_Module;

   -------------------------------------
   -- Breakpoint_Row_Selection_Change --
   -------------------------------------

   procedure Breakpoint_Row_Selection_Change
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      pragma Unreferenced (Args);

      View    : constant Breakpoint_Editor := Breakpoint_Editor (Widget);
      Process : constant Visual_Debugger := Get_Process (View);
      Model   : Gtk_Tree_Model;
      Iter    : Gtk_Tree_Iter;
      Br      : Breakpoint_Data;
      Br_Num  : Breakpoint_Identifier;

   begin
      Get_Selected (Get_Selection (View.Breakpoint_List), Model, Iter);

      if Iter = Null_Iter then
         View.Advanced_Location.Set_Sensitive (False);
         View.Remove.Set_Sensitive (False);
         View.View.Set_Sensitive (False);
         return;
      end if;

      Br_Num := Breakpoint_Identifier'Value
        (Get_String (Model, Iter, Col_Num));

      for B in Process.Breakpoints'Range loop
         if Process.Breakpoints (B).Num = Br_Num then
            Br := Process.Breakpoints (B);
            exit;
         end if;
      end loop;

      --  Fill the information

      if Br.Except /= null then
         View.Notebook.Set_Current_Page (2);
         Set_Active (View.Stop_Always_Exception, True);

         if Br.Except.all = "all" then
            Set_Active_Text (View.Exception_Name, -"All Ada exceptions");
         elsif Br.Except.all = "unhandled" then
            Set_Active_Text (View.Exception_Name, -"All Ada exceptions");
            Set_Active (View.Stop_Not_Handled_Exception, True);
         else
            Add_Unique_Combo_Entry
              (View.Exception_Name, Br.Except.all, Select_Text => True);
         end if;

         Set_Active (View.Temporary_Exception, Br.Disposition /= Keep);

      else
         View.Notebook.Set_Current_Page (0);

         if Br.File /= GNATCOLL.VFS.No_File then
            Set_Active (View.Location_Selected, True);
            Set_Text (View.File_Name, +Base_Name (Br.File));
            --  ??? What if the filesystem path is non-UTF8?
            Set_Value (View.Line_Spin, Grange_Float (Br.Line));

            if Br.Subprogram /= null then
               Add_Unique_Combo_Entry
                 (View.Subprogram_Combo, Br.Subprogram.all, True);
            end if;

         else
            Set_Active (View.Address_Selected, True);
            Add_Unique_Combo_Entry
              (View.Address_Combo, Address_To_String (Br.Address));
            Set_Text
              (Gtk_Entry (View.Address_Combo.Get_Child),
               Address_To_String (Br.Address));
         end if;
      end if;

      View.Advanced_Location.Set_Sensitive (True);
      View.Remove.Set_Sensitive (True);
      View.View.Set_Sensitive (True);
   end Breakpoint_Row_Selection_Change;

   ------------------------
   -- Breakpoint_Clicked --
   ------------------------

   function Breakpoint_Clicked
     (Widget : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean
   is
      View  : constant Breakpoint_Editor := Breakpoint_Editor (Widget);
      Iter  : Gtk_Tree_Iter;
      Col   : Gtk_Tree_View_Column;
      Model : constant Gtk_Tree_Store := -Get_Model (View.Breakpoint_List);
   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Press
      then
         Coordinates_For_Event
           (View.Breakpoint_List,
            Event, Iter, Col);

         if Iter /= Null_Iter
           and then Col = Get_Column (View.Breakpoint_List, Col_Enb)
         then
            --  Click in the second column => change the enable/disable state
            --  For efficiency, no need to reparse the list of breakpoints,
            --  since only the state of one of them as changed and we know all
            --  about it.

            Model.Set
              (Iter, Col_Enb,
               Toggle_Breakpoint_State
                 (Visual_Debugger (Get_Process (View)),
                  Breakpoint_Num => Breakpoint_Identifier'Value
                    (Get_String (Model, Iter, Col_Num))));

            --  Stop propagation of the current signal, to avoid extra calls
            --  to Select/Unselect row.

            return True;
         end if;

      elsif Event.Button = 1
        and then Event.The_Type = Gdk_2button_Press
      then
         On_View_Clicked (Gtk_Widget (Widget));
      end if;

      return False;
   end Breakpoint_Clicked;

   -------------------------
   -- Get_Selection_Index --
   -------------------------

   function Get_Selection_Index
     (View : access Breakpoint_Editor_Record'Class) return Integer
   is
      Process   : constant Visual_Debugger := Get_Process (View);
      Br_Num    : Breakpoint_Identifier;
      Iter      : Gtk_Tree_Iter;
      The_Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (View.Breakpoint_List), The_Model, Iter);

      if Iter /= Null_Iter then
         Br_Num := Breakpoint_Identifier'Value
           (Get_String (The_Model, Iter, Col_Num));

         for B in Process.Breakpoints'Range loop
            if Process.Breakpoints (B).Num = Br_Num then
               return B;
            end if;
         end loop;
      end if;

      return -1;
   end Get_Selection_Index;

   --------------------------
   -- Fill_Advanced_Dialog --
   --------------------------

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data)
   is
      Start, The_End : Gtk_Text_Iter;
      Buffer         : Gtk_Text_Buffer;

   begin
      if Advanced = null then
         return;
      end if;

      if Br.Condition /= null then
         Add_Unique_Combo_Entry
           (Advanced.Condition_Combo, Br.Condition.all, Select_Text => True);
      else
         Advanced.Condition_Combo.Set_Active (-1);
      end if;

      Set_Value (Advanced.Ignore_Count_Combo, Grange_Float (Br.Ignore));

      Buffer := Get_Buffer (Advanced.Command_Descr);

      Get_Bounds (Buffer, Start, The_End);
      Delete (Buffer, Start, The_End);

      if Br.Commands /= null then
         Insert_At_Cursor (Buffer, Br.Commands.all);
      end if;

      --  Set the scope and action, if appropriate
      case Br.Scope is
         when No_Scope =>
            null;
         when Current_Task =>
            Set_Active (Advanced.Scope_Task, True);
         when Tasks_In_PD =>
            Set_Active (Advanced.Scope_Pd, True);
         when Any_Task =>
            Set_Active (Advanced.Scope_Any, True);
      end case;

      case Br.Action is
         when No_Action =>
            null;
         when Current_Task =>
            Set_Active (Advanced.Action_Task, True);
         when Tasks_In_PD =>
            Set_Active (Advanced.Action_Pd, True);
         when All_Tasks =>
            Set_Active (Advanced.Action_All, True);
      end case;

      Set_Active (Advanced.Set_Default, False);
   end Fill_Advanced_Dialog;

   ------------------
   -- Set_Advanced --
   ------------------

   procedure Set_Advanced
     (View : access Breakpoint_Editor_Record'Class;
      Br   : Breakpoint_Data)
   is
      Adv  : constant Advanced_Breakpoint_Access := View.Advanced_Breakpoints;
      Process : constant Visual_Debugger := Get_Process (View);
      Modified : Boolean := False;
      Start, The_End : Gtk_Text_Iter;

   begin
      if Adv.Condition_Box.Get_Visible then
         Get_Bounds (Get_Buffer (Adv.Command_Descr), Start, The_End);

         declare
            S : constant String :=
                  Get_Active_Text (Adv.Condition_Combo);
            C : constant Integer :=
                  Integer (Get_Value_As_Int (Adv.Ignore_Count_Combo));
            T : constant String := Get_Text
              (Get_Buffer (Adv.Command_Descr), Start, The_End);

         begin
            --  Send all these commands in "internal" mode, so that no
            --  "info breakpoint" is emitted each time. However, we must
            --  make sure to send at least one.

            if S /= ""
              or else (Br.Condition /= null and then Br.Condition.all /= "")
            then
               Set_Breakpoint_Condition
                 (Process.Debugger, Br.Num, S, Internal);
               Modified := True;
            end if;

            if C /= 0
              or else Br.Ignore /= 0
            then
               Set_Breakpoint_Ignore_Count
                 (Process.Debugger, Br.Num, C, Internal);
               Modified := True;
            end if;

            if T /= ""
              or else (Br.Commands /= null and then Br.Commands.all /= "")
            then
               Set_Breakpoint_Command
                 (Process.Debugger, Br.Num, T, Internal);
               Modified := True;
            end if;

            if Modified then
               Update_Breakpoints (Process, Force => True);
            end if;
         end;
      end if;
   end Set_Advanced;

   ------------------------------
   -- On_Add_Exception_Clicked --
   ------------------------------

   procedure On_Add_Exception_Clicked (W : access GObject_Record'Class) is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (W);
      Temporary : Boolean;
      Process   : constant Visual_Debugger := Get_Process (View);
      Name      : constant String :=
                    Get_Text (Gtk_Entry (View.Exception_Name.Get_Child));
      Unhandled : constant Boolean :=
                    Get_Active (View.Stop_Not_Handled_Exception);

   begin
      Temporary := Get_Active (View.Temporary_Exception);

      --  Some of the strings below deal with the GUI, and thus should be
      --  translated for internationalization. Others come from gdb, and
      --  should not be translated.
      --  This explains why some are preceded by '-'.

      if Name = -"All Ada exceptions" then
         Break_Exception
           (Process.Debugger,
            Name      => "",
            Unhandled => Unhandled,
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);

      elsif Name = -"Ada assertions" then
         Break_Subprogram
           (Process.Debugger,
            Name      => "assert",
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);

      else
         Break_Exception
           (Process.Debugger,
            Name      => Name,
            Unhandled => Unhandled,
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;
   end On_Add_Exception_Clicked;

   -----------------------------
   -- On_Add_Location_Clicked --
   -----------------------------

   procedure On_Add_Location_Clicked (Object : access GObject_Record'Class) is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Process   : constant Visual_Debugger := Get_Process (View);
      Temporary : Boolean;

   begin
      Temporary := Get_Active (View.Temporary_Location);

      if Get_Active (View.Location_Selected) then
         declare
            File : constant Filesystem_String := +Get_Text (View.File_Name);
            --  ??? What if the filesystem path is non-UTF8?

            Line : constant Integer :=
              Integer (Get_Value_As_Int (View.Line_Spin));

         begin
            --  ??? Should also check Temporary
            Break_Source
              (Process.Debugger,
               File      => Create_From_Base (File),
               Line      => Line,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end;

      elsif Get_Active (View.Subprogram_Selected) then
         declare
            Name : constant String := Get_Active_Text (View.Subprogram_Combo);

         begin
            --  ??? Should also check Temporary
            Break_Subprogram
              (Process.Debugger,
               Name      => Name,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end;

      elsif Get_Active (View.Address_Selected) then
         declare
            Address : constant Address_Type :=
                        String_To_Address
                          (Get_Active_Text (View.Address_Combo));
         begin
            Break_Address
              (Process.Debugger,
               Address   => Address,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end;

      else
         Break_Regexp
           (Process.Debugger,
            Regexp    => Get_Active_Text (View.Regexp_Combo),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;
   end On_Add_Location_Clicked;

   ----------------------------------
   -- On_Advanced_Location_Clicked --
   ----------------------------------

   procedure On_Advanced_Location_Clicked (W : access GObject_Record'Class) is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (W);
      Current : constant Integer := Get_Selection_Index (View);
      Process : constant Visual_Debugger := Get_Process (View);
      Adv  : Advanced_Breakpoint_Access := View.Advanced_Breakpoints;
   begin
      if Current = -1 then
         return;
      end if;

      --  Create all three dialogs

      if Adv = null then
         Gtk_New (View.Advanced_Breakpoints);
         Adv := View.Advanced_Breakpoints;
         Set_Sensitive (Adv.Record_Button, False);
         Set_Sensitive (Adv.End_Button, False);
      end if;

      Fill_Advanced_Dialog (Adv, Process.Breakpoints (Current));
      Show_All (Adv);

      if VxWorks_Version (Process.Debugger) = Vx653 then
         Set_Show_Tabs (Adv.Main_Notebook);
      else
         Hide (Adv.Scope_Box);
         Set_Show_Tabs (Adv.Main_Notebook, False);
      end if;

      Adv.Response_Action := Gtk_Response_None;
      Gtk.Main.Main;

      if Adv.Response_Action = Gtk_Response_Apply then
         --  If we are using AE and the user has activated the "Set as
         --  default" checkbox for the scope and action values, send the
         --  appropriate commands to the debugger

         if VxWorks_Version (Process.Debugger) = Vx653 then
            declare
               Scope_Value  : Scope_Type;
               Action_Value : Action_Type;
            begin
               if Get_Active (Adv.Scope_Task) then
                  Scope_Value := Current_Task;
               elsif Get_Active (Adv.Scope_Pd) then
                  Scope_Value := Tasks_In_PD;
               elsif Get_Active (Adv.Scope_Any) then
                  Scope_Value := Any_Task;
               end if;

               if Get_Active (Adv.Action_Task) then
                  Action_Value := Current_Task;
               elsif Get_Active (Adv.Action_Pd) then
                  Action_Value := Tasks_In_PD;
               elsif Get_Active (Adv.Action_All) then
                  Action_Value := All_Tasks;
               end if;

               if Get_Active (Adv.Set_Default) then
                  Set_Scope_Action
                    (Process.Debugger, Scope_Value, Action_Value);
               end if;

               Set_Scope_Action
                 (Process.Debugger, Scope_Value,
                  Action_Value, Process.Breakpoints (Current).Num);
            end;
         end if;

         Set_Advanced (View, Process.Breakpoints (Current));
      end if;
   end On_Advanced_Location_Clicked;

   -------------------------------
   -- On_Add_Watchpoint_Clicked --
   -------------------------------

   procedure On_Add_Watchpoint_Clicked (W : access GObject_Record'Class) is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (W);
      Process         : constant Visual_Debugger := Get_Process (View);
      Watchpoint_Name : constant String :=
                          Get_Text (View.Watchpoint_Name);
      Watchpoint_Type : constant String :=
                          Get_Active_Text (View.Watchpoint_Type);
      Watchpoint_Cond : constant String :=
                          Get_Text (View.Watchpoint_Cond);

      Trigger : GVD.Types.Watchpoint_Trigger;
      --  Encodes the value we get from Watchpoint_Type for the call to Watch

   begin
      if Watchpoint_Type = -"read" then
         Trigger := GVD.Types.Read;
      elsif Watchpoint_Type = -"read or written" then
         Trigger := GVD.Types.Read_Write;
      else
         --  presumably, Watchpoint_Type = -"written"
         Trigger := GVD.Types.Write;
      end if;

      Watch
        (Process.Debugger,
         Name      => Watchpoint_Name,
         Trigger   => Trigger,
         Condition => Watchpoint_Cond,
         Mode      => GVD.Types.Visible);
   end On_Add_Watchpoint_Clicked;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (W : access GObject_Record'Class)
   is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (W);
      Process : constant Visual_Debugger := Get_Process (View);
   begin
      declare
         Exception_Arr : Exception_Array := List_Exceptions (Process.Debugger);
      begin
         if Exception_Arr'Length > 0 then
            Set_Sensitive (View.Hbox_Exceptions, True);
            Add_Unique_Combo_Entry
              (View.Exception_Name, -"All Ada exceptions");
            Add_Unique_Combo_Entry
              (View.Exception_Name, -"Ada assertions");

            for J in Exception_Arr'Range loop
               Add_Unique_Combo_Entry
                 (View.Exception_Name, Exception_Arr (J).Name.all);
            end loop;
         else
            Set_Sensitive (View.Hbox_Exceptions, False);
         end if;

         Free (Exception_Arr);
      end;
   end On_Load_Exception_List_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked (W : access GObject_Record'Class) is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (W);
      Process   : constant Visual_Debugger := Get_Process (View);
      Model     : constant Gtk_Tree_Store :=
        -Get_Model (View.Breakpoint_List);
      Selection : constant Integer := Get_Selection_Index (View);

   begin
      if Selection /= -1 then
         Remove_Breakpoint
           (Process.Debugger,
            Process.Breakpoints (Selection).Num,
            Mode => GVD.Types.Visible);

         --  Reselect the next line for convenience, so that the user can
         --  press "Remove" several times in a row

         if Gint (Selection) >= N_Children (Model, Null_Iter) then
            Select_Iter
              (Get_Selection (View.Breakpoint_List),
               Nth_Child
                 (Model,
                  Parent => Null_Iter,
                  N      => N_Children (Model, Null_Iter) - 1));
         else
            Select_Iter
              (Get_Selection (View.Breakpoint_List),
               Nth_Child
                 (Model,
                  Parent => Null_Iter,
                  N      => Gint (Selection)));
         end if;
      end if;
   end On_Remove_Clicked;

   ---------------------
   -- On_View_Clicked --
   ---------------------

   procedure On_View_Clicked (W : access GObject_Record'Class) is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (W);
      Process   : constant Visual_Debugger := Get_Process (View);
      Selection : constant Integer := Get_Selection_Index (View);

   begin
      if Selection /= -1 then
         Load_File
           (Process.Editor_Text,
            Process.Breakpoints (Selection).File);
         Set_Line
           (Process.Editor_Text,
            Process.Breakpoints (Selection).Line,
            GObject (Process));
      end if;
   end On_View_Clicked;

   ------------------------------------
   -- On_Breakpoints_Key_Press_Event --
   ------------------------------------

   function On_Breakpoints_Key_Press_Event
     (W      : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Key) return Boolean is
   begin
      if Event.Keyval = GDK_Delete then
         On_Remove_Clicked (W);
      end if;
      return False;
   end On_Breakpoints_Key_Press_Event;

   ----------------------------------
   -- On_Location_Selected_Toggled --
   ----------------------------------

   procedure On_Location_Selected_Toggled (W : access GObject_Record'Class) is
      Self : constant Breakpoint_Editor := Breakpoint_Editor (W);
   begin
      Self.File_Name.Set_Sensitive (True);
      Self.Line_Spin.Set_Sensitive (True);
      Self.Address_Combo.Set_Sensitive (False);
      Self.Subprogram_Combo.Set_Sensitive (False);
      Self.Regexp_Combo.Set_Sensitive (False);
   end On_Location_Selected_Toggled;

   ------------------------------------
   -- On_Subprogram_Selected_Toggled --
   ------------------------------------

   procedure On_Subprogram_Selected_Toggled
     (W : access GObject_Record'Class)
   is
      Self : constant Breakpoint_Editor := Breakpoint_Editor (W);
   begin
      Self.File_Name.Set_Sensitive (False);
      Self.Line_Spin.Set_Sensitive (False);
      Self.Address_Combo.Set_Sensitive (False);
      Self.Subprogram_Combo.Set_Sensitive (True);
      Self.Regexp_Combo.Set_Sensitive (False);
   end On_Subprogram_Selected_Toggled;

   ---------------------------------
   -- On_Address_Selected_Toggled --
   ---------------------------------

   procedure On_Address_Selected_Toggled (W : access GObject_Record'Class) is
      Self : constant Breakpoint_Editor := Breakpoint_Editor (W);
   begin
      Self.File_Name.Set_Sensitive (False);
      Self.Line_Spin.Set_Sensitive (False);
      Self.Address_Combo.Set_Sensitive (True);
      Self.Subprogram_Combo.Set_Sensitive (False);
      Self.Regexp_Combo.Set_Sensitive (False);
   end On_Address_Selected_Toggled;

   --------------------------------
   -- On_Regexp_Selected_Toggled --
   --------------------------------

   procedure On_Regexp_Selected_Toggled (W : access GObject_Record'Class) is
      Self : constant Breakpoint_Editor := Breakpoint_Editor (W);
   begin
      Self.File_Name.Set_Sensitive (False);
      Self.Line_Spin.Set_Sensitive (False);
      Self.Address_Combo.Set_Sensitive (False);
      Self.Subprogram_Combo.Set_Sensitive (False);
      Self.Regexp_Combo.Set_Sensitive (True);
   end On_Regexp_Selected_Toggled;

end GVD.Breakpoints;
