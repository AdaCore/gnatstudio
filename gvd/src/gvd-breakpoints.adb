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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Debugger;                  use Debugger;
with Gdk.Event;                 use Gdk.Event;
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
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks; use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Main_Window;           use GPS.Main_Window;
with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Label;                 use Gtk.Label;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Spin_Button;           use Gtk.Spin_Button;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;
with GUI_Utils;                 use GUI_Utils;
with GVD;                       use GVD;
with GVD.Code_Editors;          use GVD.Code_Editors;
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

   Column_Names : constant GNAT.Strings.String_List (1 .. 8) :=
     (new String'("Num"),
      new String'("Enb"),
      new String'("Type"),
      new String'("Disp"),
      new String'("File/Variable"),
      new String'("Line"),
      new String'("Exception"),
      new String'("Subprograms"));

   type Breakpoint_Type is
     (Break_On_Source_Loc,
      Break_On_Subprogram,
      Break_At_Address,
      Break_On_Regexp,
      Break_On_Variable,
      Break_On_Exception);
   --  The various types of breakpoints

   type Breakpoint_Editor_Record is new Process_View_Record with
      record
         Breakpoint_List      : Gtk_Tree_View;
      end record;
   type Breakpoint_Editor is access all Breakpoint_Editor_Record'Class;

   overriding procedure Update
     (View   : not null access Breakpoint_Editor_Record);
   overriding procedure On_Process_Terminated
     (View : not null access Breakpoint_Editor_Record);
   --  See inherited documentation

   type Properties_Editor_Record is new Gtk_Dialog_Record with record
      Process              : access Visual_Debugger_Record'Class;

      Temporary            : Gtk_Check_Button;

      Location_Box         : Gtk_Box;
      Subprogram_Box       : Gtk_Box;
      Address_Box          : Gtk_Box;
      Regexp_Box           : Gtk_Box;
      Variable_Box         : Gtk_Box;
      Exception_Box        : Gtk_Box;

      Stop_Always_Exception      : Gtk_Radio_Button;
      Stop_Not_Handled_Exception : Gtk_Radio_Button;

      Breakpoint_Type      : Gtk_Combo_Box_Text;

      Exception_Name       : Gtk_Combo_Box_Text;

      Watchpoint_Name      : Gtk_Entry;
      Watchpoint_Type      : Gtk_Combo_Box_Text;
      Watchpoint_Cond      : Gtk_Entry;

      File_Name            : Gtk_Entry;
      Line_Spin            : Gtk_Spin_Button;
      Address_Combo        : Gtk_Combo_Box_Text;
      Subprogram_Combo     : Gtk_Combo_Box_Text;
      Regexp_Combo         : Gtk_Combo_Box_Text;

      Condition_Frame      : Gtk_Frame;
      Condition_Combo      : Gtk_Combo_Box_Text;

      Ignore_Frame         : Gtk_Frame;
      Ignore_Count_Combo   : Gtk_Spin_Button;

      Command_Frame        : Gtk_Frame;
      Command_Descr        : Gtk_Text_View;

      Scope_Frame          : Gtk_Frame;
      Scope_Task           : Gtk_Radio_Button;
      Scope_Pd             : Gtk_Radio_Button;
      Scope_Any            : Gtk_Radio_Button;

      Task_Frame           : Gtk_Frame;
      Action_Task          : Gtk_Radio_Button;
      Action_Pd            : Gtk_Radio_Button;
      Action_All           : Gtk_Radio_Button;

      Set_Default          : Gtk_Check_Button;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Initialize
     (Self    : not null access Properties_Editor_Record'Class;
      Process : not null access Visual_Debugger_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class);
   --  Create the breakpoint properties editor

   procedure Fill
     (Self    : not null access Properties_Editor_Record'Class;
      Br      : Breakpoint_Data);
   --  Show the information for the given breakpoint in the editor

   procedure Apply
     (Self    : not null access Properties_Editor_Record'Class;
      Br      : in out Breakpoint_Data);
   --  Apply the settings to the given breakpoint

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
      Local_Toolbar      => True,
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

   function Breakpoint_Clicked
     (Widget : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   --  Called when receiving a click on the breakpoint tree.

   type Remove_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove the selected breakpoint

   type View_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access View_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Show the source editor that has the breakpoint

   type Advanced_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Advanced_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit the advanced properties of the selected breakpoint

   type Add_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Create a new breakpoint

   procedure On_Load_Exception_List_Clicked (W : access GObject_Record'Class);
   procedure On_Type_Changed                (W : access GObject_Record'Class);
   --  Callbacks for the various buttons

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

         if Br.Expression /= "" then
            Last := Last + 1;
            Columns (Last) := Col_File;
            Glib.Values.Init_Set_String
              (Values (Last), To_String (Br.Expression));
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

         if Br.Except /= "" then
            Last := Last + 1;
            Columns (Last) := Col_Exception;
            Glib.Values.Init_Set_String (Values (Last), To_String (Br.Except));
         end if;

         if Br.Subprogram /= "" then
            Last := Last + 1;
            Columns (Last) := Col_Subprogs;
            Glib.Values.Init_Set_String
              (Values (Last), To_String (Br.Subprogram));
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
   begin
      --  If the debugger was killed, no need to refresh

      if V /= null and then Get_Process (V.Debugger) /= null then
         Update_Breakpoint_List (View);
      end if;
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
      Main_Vbox  : Gtk_Box;
      Scroll     : Gtk_Scrolled_Window;
   begin
      Gtk.Box.Initialize_Hbox (Self);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scroll);

      Gtk_New_Vbox (Main_Vbox);
      Scroll.Add_With_Viewport (Main_Vbox);

      ----------
      --  List of breakpoints
      ----------

      Self.Breakpoint_List := Create_Tree_View
        (Column_Types, Column_Names, Sortable_Columns => False);
      Self.Breakpoint_List.On_Button_Press_Event
        (Breakpoint_Clicked'Access, Self);
      Main_Vbox.Pack_Start (Self.Breakpoint_List);

      Debugger_Breakpoints_Changed_Hook.Add
        (new On_Breakpoints_Changed, Watch => Self);

      return Gtk_Widget (Self.Breakpoint_List);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : not null access Properties_Editor_Record'Class;
      Process : not null access Visual_Debugger_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class)
   is
      Button     : Gtk_Button;
      Frame      : Gtk_Frame;
      Label      : Gtk_Label;
      Scroll     : Gtk_Scrolled_Window;
      Vbox9      : Gtk_Vbox;
      Details    : Gtk_Vbox;
      Hbox       : Gtk_Box;
      Size       : Gtk_Size_Group;
      Adj        : Gtk_Adjustment;
      M          : Gtk_List_Store;
      Dummy      : Gtk_Widget;
   begin
      Gtk.Dialog.Initialize
        (Self,
         -"Breakpoint editor",
         Parent => Kernel.Get_Main_Window,
         Flags  => Destroy_With_Parent);

      Set_Default_Size_From_History
        (Self, "breakpoints", Kernel, 600, 500);

      Self.Process := Process;

      ------------
      --  Chosing the type of the breakpoint
      ------------

      Gtk_New (Self.Breakpoint_Type);
      for T in Breakpoint_Type loop
         case T is
            when Break_On_Source_Loc =>
               Self.Breakpoint_Type.Append_Text (-"break on source location");
            when Break_On_Subprogram =>
               Self.Breakpoint_Type.Append_Text (-"break on subprogram");
            when Break_At_Address =>
               Self.Breakpoint_Type.Append_Text (-"break at specific address");
            when Break_On_Regexp =>
               Self.Breakpoint_Type.Append_Text
                 (-"break on regular expression");
            when Break_On_Variable =>
               Self.Breakpoint_Type.Append_Text (-"watch changes on variable");
            when Break_On_Exception =>
               Self.Breakpoint_Type.Append_Text (-"break on exception");
         end case;
      end loop;

      Self.Breakpoint_Type.Set_Active
        (Breakpoint_Type'Pos (Break_On_Source_Loc));

      Self.Breakpoint_Type.On_Changed (On_Type_Changed'Access, Self);

      ------------
      --  Breakpoint details
      ------------

      Gtk_New (Frame);
      Frame.Set_Label_Widget (Self.Breakpoint_Type);
      Self.Get_Content_Area.Pack_Start (Frame, Expand => False, Fill => False);

      Gtk_New_Vbox (Details);
      Frame.Add (Details);

      ------------
      --  Break on source location
      ------------

      Gtk_New_Vbox (Self.Location_Box);
      Details.Pack_Start (Self.Location_Box, Expand => True, Fill => True);

      Gtk_New (Size);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Location_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, -("File:"));
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False, 0);
      Size.Add_Widget (Label);

      Gtk_New (Self.File_Name);
      Self.File_Name.Set_Editable (True);
      Hbox.Pack_Start (Self.File_Name, True, True, 0);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Location_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, -("Line:"));
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False, 0);
      Size.Add_Widget (Label);

      Gtk_New (Self.Line_Spin, Min => 1.0, Max => 1.0e+08, Step => 1.0);
      Self.Line_Spin.Set_Numeric (True);
      Self.Line_Spin.Set_Value (1.0);
      Hbox.Pack_Start (Self.Line_Spin, True, True, 0);

      ------------
      --   Break on subprogram
      ------------

      Gtk_New_Vbox (Self.Subprogram_Box);
      Details.Pack_Start (Self.Subprogram_Box, Expand => True, Fill => True);

      Gtk_New_With_Entry (Self.Subprogram_Combo);
      Self.Subprogram_Combo.Append_Text ("");
      Self.Subprogram_Box.Pack_Start (Self.Subprogram_Combo, False, False);

      ------------
      --  Break_On_Address
      ------------

      Gtk_New_Vbox (Self.Address_Box);
      Details.Pack_Start (Self.Address_Box, Expand => True, Fill => True);

      Gtk_New_With_Entry (Self.Address_Combo);
      Self.Address_Combo.Append_Text ("");
      Self.Address_Box.Pack_Start (Self.Address_Combo, False, False);

      ------------
      --  Break on regexp
      ------------

      Gtk_New_Vbox (Self.Regexp_Box);
      Details.Pack_Start (Self.Regexp_Box, Expand => True, Fill => True);

      Gtk_New_With_Entry (Self.Regexp_Combo);
      Self.Regexp_Combo.Append_Text ("");
      Self.Regexp_Box.Pack_Start (Self.Regexp_Combo, False, False);

      ------------
      --  Break on variable
      ------------

      Gtk_New_Vbox (Self.Variable_Box);
      Details.Pack_Start (Self.Variable_Box, Expand => True, Fill => True);

      Gtk_New (Size);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Variable_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, -("Break when variable:"));
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False);
      Size.Add_Widget (Label);

      Gtk_New (Self.Watchpoint_Name);
      Hbox.Pack_Start (Self.Watchpoint_Name, True, True);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Variable_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, -("is"));
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False);
      Size.Add_Widget (Label);

      Gtk_New (Self.Watchpoint_Type);
      Self.Watchpoint_Type.Append_Text (-"written");
      Self.Watchpoint_Type.Append_Text (-"read");
      Self.Watchpoint_Type.Append_Text (-"read or written");
      Self.Watchpoint_Type.Set_Active (1); --  "read"
      Hbox.Pack_Start (Self.Watchpoint_Type, True, True);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Variable_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, -("Condition:"));
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False);
      Size.Add_Widget (Label);

      Gtk_New (Self.Watchpoint_Cond);
      Hbox.Pack_Start (Self.Watchpoint_Cond, True, True);

      -----------
      --  Break on exception
      -----------

      Gtk_New_Vbox (Self.Exception_Box);
      Details.Pack_Start (Self.Exception_Box, Expand => True, Fill => True);

      Gtk_New_Hbox (Hbox, False, 8);
      Self.Exception_Box.Pack_Start (Hbox, False, True, 0);

      Gtk_New_With_Entry (Self.Exception_Name);
      Self.Exception_Name.Append_Text ("All Ada exceptions");
      Self.Exception_Name.Set_Active (0);
      Hbox.Pack_Start (Self.Exception_Name, True, True, 0);

      Gtk_New (Button, -"Load List");
      Hbox.Pack_Start (Button, False, False, 0);
      Button.On_Clicked (On_Load_Exception_List_Clicked'Access, Self);

      Gtk_New (Frame, -"Action");
      Self.Exception_Box.Pack_Start (Frame, False, False, 7);

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

      -------------
      --  A temporary breakpoint ?
      -------------

      Gtk_New (Self.Temporary, -"Temporary breakpoint");
      Self.Temporary.Set_Active (False);
      Details.Pack_Start (Self.Temporary, False, False, 5);

      --------------
      --  Advanced: condition
      --------------

      Gtk_New (Self.Condition_Frame, -"Condition");
      Details.Pack_Start (Self.Condition_Frame, False, True, 0);

      Gtk_New_Vbox (Vbox9, False, 0);
      Self.Condition_Frame.Add (Vbox9);

      Gtk_New (Label, -"Break only when following condition is true:");
      Label.Set_Alignment (0.0, 0.5);
      Vbox9.Pack_Start (Label, False);

      Gtk_New_With_Entry (Self.Condition_Combo);
      Vbox9.Pack_Start (Self.Condition_Combo, False, False, 0);

      --------------
      --  Advanced: ignore
      --------------

      Gtk_New (Self.Ignore_Frame, -"Ignore");
      Details.Pack_Start (Self.Ignore_Frame, False, True, 0);

      Gtk_New_Vbox (Vbox9, False, 0);
      Self.Ignore_Frame.Add (Vbox9);

      Gtk_New (Label, -"Enter the number of times to skip before stopping:");
      Label.Set_Alignment (0.0, 0.5);
      Vbox9.Pack_Start (Label, False, False, 0);

      Gtk_New (Adj, 0.0, 0.0, 10000.0, 1.0, 10.0);
      Gtk_New (Self.Ignore_Count_Combo, Adj, 1.0, 0);
      Self.Ignore_Count_Combo.Set_Numeric (False);
      Self.Ignore_Count_Combo.Set_Snap_To_Ticks (True);
      Self.Ignore_Count_Combo.Set_Update_Policy (Update_Always);
      Self.Ignore_Count_Combo.Set_Value (0.0);
      Self.Ignore_Count_Combo.Set_Wrap (False);
      Vbox9.Pack_Start (Self.Ignore_Count_Combo, False, False, 0);

      --------------
      --  Advanced: commands
      --------------

      Gtk_New (Self.Command_Frame, -"Commands");
      Details.Pack_Start (Self.Command_Frame, False, True, 0);

      Gtk_New_Vbox (Vbox9, False, 0);
      Self.Command_Frame.Add (Vbox9);

      Gtk_New (Label, -"Enter commands to execute when program stops:");
      Label.Set_Alignment (0.0, 0.5);
      Vbox9.Pack_Start (Label, False, False, 0);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Vbox9.Pack_Start (Scroll, False, False, 0);

      Gtk_New (Self.Command_Descr);
      Scroll.Add (Self.Command_Descr);

      ---------------
      --  Advanced: tasks scope
      --------------

      Gtk_New (Self.Scope_Frame, -"Scope: tasks that can hit the breakpoint");
      Details.Pack_Start (Self.Scope_Frame, False, True, 5);

      Gtk_New_Vbox (Vbox9, True, 3);
      Self.Scope_Frame.Add (Vbox9);

      Gtk_New (Self.Scope_Task, null, -"Running task");
      Self.Scope_Task.Set_Active (True);
      Vbox9.Pack_Start (Self.Scope_Task, False, False, 0);

      Gtk_New (Self.Scope_Pd, Self.Scope_Task,
               -"All tasks in current Protection Domain");
      Vbox9.Pack_Start (Self.Scope_Pd, False, False, 0);

      Gtk_New (Self.Scope_Any, Self.Scope_Task, -"Any task");
      Vbox9.Pack_Start (Self.Scope_Any, False, False, 0);

      ----------------
      --  Advanced: tasks to break
      ----------------

      Gtk_New (Self.Task_Frame, -"Action: tasks to break");
      Details.Pack_Start (Self.Task_Frame, False, True, 0);

      Gtk_New_Vbox (Vbox9, True, 3);
      Self.Task_Frame.Add (Vbox9);

      Gtk_New (Self.Action_Task, null, -"Task that hits the breakpoint");
      Self.Action_Task.Set_Active (True);
      Vbox9.Pack_Start (Self.Action_Task, False, False, 0);

      Gtk_New (Self.Action_Pd, Self.Action_Task,
               -"All tasks in current Protection Domain");
      Self.Action_Pd.Set_Active (False);
      Vbox9.Pack_Start (Self.Action_Pd, False, False, 0);

      Gtk_New (Self.Action_All, Self.Action_Task, -"All breakable tasks");
      Self.Action_All.Set_Active (False);
      Vbox9.Pack_Start (Self.Action_All, False, False, 0);

      --------------
      --  Session's default
      --------------

      Gtk_New (Self.Set_Default, -"Set these values as session's default");
      Self.Set_Default.Set_Active (False);
      Details.Pack_Start (Self.Set_Default, False, False, 2);

      --------------
      --  Set proper visibility
      --------------

      Self.Show_All;
      Self.Location_Box.Set_No_Show_All (True);
      Self.Subprogram_Box.Set_No_Show_All (True);
      Self.Address_Box.Set_No_Show_All (True);
      Self.Regexp_Box.Set_No_Show_All (True);
      Self.Variable_Box.Set_No_Show_All (True);
      Self.Exception_Box.Set_No_Show_All (True);

      Self.Scope_Frame.Set_No_Show_All (True);
      Self.Task_Frame.Set_No_Show_All (True);

      if True or else VxWorks_Version (Process.Debugger) /= Vx653 then
         Self.Scope_Frame.Hide;
         Self.Task_Frame.Hide;
      end if;

      --------------
      --  Fill information
      --------------

      --  Reinitialize the contents of the file name entry
      Set_Text
        (Self.File_Name, +Base_Name (Get_Current_File (Process.Editor_Text)));
      --  ??? What if the filesystem path is non-UTF8?

      --  Clear the contents of the exceptions combo (its contents is in fact
      --  cached in gdb, so it is fast enough to call "info exceptions" again)
      M := -Self.Exception_Name.Get_Model;
      M.Clear;
      Add_Unique_Combo_Entry (Self.Exception_Name, -"All Ada exceptions");
      Add_Unique_Combo_Entry (Self.Exception_Name, -"Ada assertions");

      On_Type_Changed (Self);

      ----------------
      --  Action buttons
      ----------------

      Dummy := Self.Add_Button (Stock_Ok, Gtk_Response_Apply);
      Dummy.Grab_Default;
      Dummy := Self.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter : Action_Filter;
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open breakpoints editor",
         Description => -"Open the Breakpoints Editor for the debugger");

      Filter := Create
        (Module => Breakpoints_MDI_Views.Get_Module.Module_Name);

      Register_Action
        (Kernel,
         "debug delete breakpoint", new Remove_Breakpoint_Command,
         -("Delete the currently selected breakpoint"
           & " (from the Breakpoints view)"),
         Icon_Name => "gps-remove-symbolic",
         Filter    => Filter,
         Category  => -"Debug");

      Register_Action
        (Kernel,
         "debug view breakpoint", new View_Breakpoint_Command,
         -("View the source editor containing the selected breakpoint"
           & " (from the Breakpoints view)"),
         Icon_Name => "gps-edit-symbolic",
         Filter    => Filter,
         Category  => -"Debug");

      Register_Action
        (Kernel,
         "debug edit breakpoint", new Advanced_Command,
         -("Edit the advanced properties of the selected breakpoints"
           & " like its condition, repeat count,..."
           & " (from the Breakpoints view)"),
         Icon_Name => "gps-settings-symbolic",
         Filter    => Filter,
         Category  => -"Debug");

      Register_Action
        (Kernel,
         "debug create breakpoint", new Add_Command,
         -"Create a new breakpoint, from the Breakpoints view",
         Icon_Name => "gps-add-symbolic",
         Filter    => Filter,
         Category  => -"Debug");
   end Register_Module;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Self    : not null access Properties_Editor_Record'Class;
      Br      : Breakpoint_Data)
   is
      Start, The_End : Gtk_Text_Iter;
      Buffer         : Gtk_Text_Buffer;
   begin
      --  Fill the information

      if Br.Except /= "" then
         Self.Breakpoint_Type.Set_Active
           (Breakpoint_Type'Pos (Break_On_Exception));
         Set_Active (Self.Stop_Always_Exception, True);

         if Br.Except = "all" then
            Set_Active_Text (Self.Exception_Name, -"All Ada exceptions");
         elsif Br.Except = "unhandled" then
            Set_Active_Text (Self.Exception_Name, -"All Ada exceptions");
            Set_Active (Self.Stop_Not_Handled_Exception, True);
         else
            Add_Unique_Combo_Entry
              (Self.Exception_Name,
               To_String (Br.Except), Select_Text => True);
         end if;

         Set_Active (Self.Temporary, Br.Disposition /= Keep);

      elsif Br.File /= GNATCOLL.VFS.No_File
        or else Br.Num = 0   --  a new breakpoint
      then
         Self.Breakpoint_Type.Set_Active
           (Breakpoint_Type'Pos (Break_On_Source_Loc));

         Set_Text (Self.File_Name, +Base_Name (Br.File));
         --  ??? What if the filesystem path is non-UTF8?
         Set_Value (Self.Line_Spin, Grange_Float (Br.Line));

         if Br.Subprogram /= "" then
            Add_Unique_Combo_Entry
              (Self.Subprogram_Combo, To_String (Br.Subprogram), True);
         end if;

      else
         Self.Breakpoint_Type.Set_Active
           (Breakpoint_Type'Pos (Break_At_Address));

         Add_Unique_Combo_Entry
           (Self.Address_Combo, Address_To_String (Br.Address));
         Set_Text
           (Gtk_Entry (Self.Address_Combo.Get_Child),
            Address_To_String (Br.Address));
      end if;

      --  When editing an existing breakpoint, we can't change its type
      if Br.Num /= 0 then
         Self.Breakpoint_Type.Set_Sensitive (False);
         Self.Stop_Always_Exception.Set_Sensitive (False);
         Self.Exception_Name.Set_Sensitive (False);
         Self.Temporary.Set_Sensitive (False);
         Self.File_Name.Set_Sensitive (False);
         Self.Line_Spin.Set_Sensitive (False);
         Self.Subprogram_Combo.Set_Sensitive (False);
         Self.Address_Combo.Set_Sensitive (False);
      end if;

      --  Advanced: condition

      if Br.Condition /= "" then
         Add_Unique_Combo_Entry
           (Self.Condition_Combo,
            To_String (Br.Condition), Select_Text => True);
      else
         Self.Condition_Combo.Set_Active (-1);
      end if;

      --  Advanced: ignore

      Self.Ignore_Count_Combo.Set_Value (Grange_Float (Br.Ignore));

      --  Advanced: commands

      Buffer := Get_Buffer (Self.Command_Descr);
      Get_Bounds (Buffer, Start, The_End);
      Delete (Buffer, Start, The_End);

      if Br.Commands /= "" then
         Insert_At_Cursor (Buffer, To_String (Br.Commands));
      end if;

      --  Set the scope and action, if appropriate
      case Br.Scope is
         when No_Scope     => null;
         when Current_Task => Self.Scope_Task.Set_Active (True);
         when Tasks_In_PD  => Self.Scope_Pd.Set_Active (True);
         when Any_Task     => Self.Scope_Any.Set_Active (True);
      end case;

      case Br.Action is
         when No_Action    => null;
         when Current_Task => Self.Action_Task.Set_Active (True);
         when Tasks_In_PD  => Self.Action_Pd.Set_Active (True);
         when All_Tasks    => Self.Action_All.Set_Active (True);
      end case;

      Self.Set_Default.Set_Active (False);
   end Fill;

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

            Update_Breakpoints (Get_Process (View), Force => True);

            --  Stop propagation of the current signal, to avoid extra calls
            --  to Select/Unselect row.

            return True;
         end if;
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

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Self    : not null access Properties_Editor_Record'Class;
      Br      : in out Breakpoint_Data)
   is
      Modified       : Boolean := False;
      Start, The_End : Gtk_Text_Iter;
      C              : Integer;
      Scope_Value    : Scope_Type;
      Action_Value   : Action_Type;
      T              : constant Breakpoint_Type :=
        Breakpoint_Type'Val (Get_Active (Self.Breakpoint_Type));
      Temporary      : constant Boolean := Self.Temporary.Get_Active;
      Num    : Breakpoint_Identifier := Br.Num;
   begin
      --  Create a new breakpoint if needed

      if Num = 0 then
         case T is
         when Break_On_Source_Loc =>
            declare
               File : constant Filesystem_String := +Get_Text (Self.File_Name);
               --  ??? What if the filesystem path is non-UTF8?
            begin
               --  ??? Should also check Temporary
               Num := Break_Source
                 (Self.Process.Debugger,
                  File      => Create_From_Base (File),
                  Line      => Integer'Value (Self.Line_Spin.Get_Text),
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end;

         when Break_On_Subprogram =>
            Num := Break_Subprogram
              (Self.Process.Debugger,
               Name      => Self.Subprogram_Combo.Get_Active_Text,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);

         when Break_At_Address =>
            Num := Break_Address
              (Self.Process.Debugger,
               Address   =>
                  String_To_Address (Self.Address_Combo.Get_Active_Text),
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);

         when Break_On_Regexp =>
            Num := Break_Regexp
              (Self.Process.Debugger,
               Regexp    => Self.Regexp_Combo.Get_Active_Text,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);

         when Break_On_Variable =>
            declare
               Watchpoint_Name : constant String :=
                 Self.Watchpoint_Name.Get_Text;
               Watchpoint_Type : constant String :=
                 Self.Watchpoint_Type.Get_Active_Text;
               Watchpoint_Cond : constant String :=
                 Self.Watchpoint_Cond.Get_Text;
               Trigger         : constant GVD.Types.Watchpoint_Trigger :=
                 (if Watchpoint_Type = -"read" then GVD.Types.Read
                  elsif Watchpoint_Type = -"read or written"
                  then GVD.Types.Read_Write
                  else GVD.Types.Write);
            begin
               Num := Watch
                 (Self.Process.Debugger,
                  Name      => Watchpoint_Name,
                  Trigger   => Trigger,
                  Condition => Watchpoint_Cond,
                  Mode      => GVD.Types.Visible);
            end;

         when Break_On_Exception =>
            declare
               Name      : constant String :=
                 Get_Text (Gtk_Entry (Self.Exception_Name.Get_Child));
               Unhandled : constant Boolean :=
                 Get_Active (Self.Stop_Not_Handled_Exception);
            begin
               --  Some of the strings below deal with the GUI, and thus should
               --  be translated for internationalization. Others come from
               --  gdb, and should not be translated. This explains why some
               --  are preceded by '-'.

               if Name = -"All Ada exceptions" then
                  Num := Break_Exception
                    (Self.Process.Debugger,
                     Name      => "",
                     Unhandled => Unhandled,
                     Temporary => Temporary,
                     Mode      => GVD.Types.Visible);

               elsif Name = -"Ada assertions" then
                  Num := Break_Subprogram
                    (Self.Process.Debugger,
                     Name      => "assert",
                     Temporary => Temporary,
                     Mode      => GVD.Types.Visible);

               else
                  Num := Break_Exception
                    (Self.Process.Debugger,
                     Name      => Name,
                     Unhandled => Unhandled,
                     Temporary => Temporary,
                     Mode      => GVD.Types.Visible);
               end if;
            end;
         end case;
      end if;

      --  Send all these commands in "internal" mode, so that no
      --  "info breakpoint" is emitted each time. However, we must
      --  make sure to send at least one.

      if Self.Condition_Frame.Get_Visible then
         declare
            S : constant String := Self.Condition_Combo.Get_Active_Text;
         begin
            if S /= "" or else Br.Condition /= "" then
               Set_Breakpoint_Condition
                 (Self.Process.Debugger, Num, S, Internal);
               Modified := True;
            end if;
         end;
      end if;

      if Self.Command_Frame.Get_Visible then
         Get_Bounds (Get_Buffer (Self.Command_Descr), Start, The_End);
         declare
            T : constant String := Get_Text
              (Get_Buffer (Self.Command_Descr), Start, The_End);
         begin
            if T /= "" or else Br.Commands /= "" then
               Set_Breakpoint_Command
                 (Self.Process.Debugger, Num, T, Internal);
               Modified := True;
            end if;
         end;
      end if;

      if Self.Ignore_Frame.Get_Visible then
         C := Integer (Get_Value_As_Int (Self.Ignore_Count_Combo));
         if C /= 0 or else Br.Ignore /= 0 then
            Set_Breakpoint_Ignore_Count
              (Self.Process.Debugger, Num, C, Internal);
            Modified := True;
         end if;
      end if;

      --  If we are using AE and the user has activated the "Set as
      --  default" checkbox for the scope and action values, send the
      --  appropriate commands to the debugger

      if VxWorks_Version (Self.Process.Debugger) = Vx653 then
         if Self.Scope_Task.Get_Active then
            Scope_Value := Current_Task;
         elsif Self.Scope_Pd.Get_Active then
            Scope_Value := Tasks_In_PD;
         elsif Self.Scope_Any.Get_Active then
            Scope_Value := Any_Task;
         end if;

         if Self.Action_Task.Get_Active then
            Action_Value := Current_Task;
         elsif Self.Action_Pd.Get_Active then
            Action_Value := Tasks_In_PD;
         elsif Self.Action_All.Get_Active then
            Action_Value := All_Tasks;
         end if;

         if Self.Set_Default.Get_Active then
            Set_Scope_Action
              (Self.Process.Debugger, Scope_Value, Action_Value);
         end if;

         Set_Scope_Action
           (Self.Process.Debugger, Scope_Value, Action_Value, Num);
      end if;

      if Modified then
         Update_Breakpoints (Self.Process, Force => True);
      end if;
   end Apply;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Breakpoint_Editor :=
        Breakpoint_Editor
          (Breakpoints_MDI_Views.Retrieve_View (Get_Kernel (Context.Context)));
      Props : Properties_Editor;
      Br : Breakpoint_Data := (Num => 0, others => <>);
   begin
      if View /= null then
         Props := new Properties_Editor_Record;
         Initialize (Props, Visual_Debugger (Get_Process (View)), View.Kernel);
         Fill (Props, Br);

         if Props.Run = Gtk_Response_Apply then
            Apply (Props, Br);
         end if;

         --  No need to free Br: either it has not been created, or it was set
         --  by Apply as a copy of an internal field in Process.Breakpoints

         Props.Destroy;
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Advanced_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Breakpoint_Editor :=
        Breakpoint_Editor
          (Breakpoints_MDI_Views.Retrieve_View (Get_Kernel (Context.Context)));
      Current : Integer;
      Process : Visual_Debugger;
      Props   : Properties_Editor;
   begin
      if View /= null then
         Current := Get_Selection_Index (View);
         if Current /= -1 then
            Process := Get_Process (View);

            Props := new Properties_Editor_Record;
            Initialize (Props, Process, View.Kernel);
            Fill (Props, Process.Breakpoints (Current));

            if Props.Run = Gtk_Response_Apply then
               Apply (Props, Process.Breakpoints (Current));
            end if;

            Props.Destroy;
         end if;
      end if;
      return Success;
   end Execute;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (W : access GObject_Record'Class)
   is
      View    : constant Properties_Editor := Properties_Editor (W);
      Exception_Arr : constant Exception_Array :=
        List_Exceptions (View.Process.Debugger);
   begin
      if Exception_Arr'Length > 0 then
         Add_Unique_Combo_Entry
           (View.Exception_Name, -"All Ada exceptions");
         Add_Unique_Combo_Entry
           (View.Exception_Name, -"Ada assertions");

         for J in Exception_Arr'Range loop
            Add_Unique_Combo_Entry
              (View.Exception_Name, To_String (Exception_Arr (J).Name));
         end loop;
      end if;
   end On_Load_Exception_List_Clicked;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Breakpoint_Editor :=
        Breakpoint_Editor
          (Breakpoints_MDI_Views.Retrieve_View (Get_Kernel (Context.Context)));
      Process   : Visual_Debugger;
      Model     : Gtk_Tree_Store;
      Selection : Integer;
   begin
      if View /= null then
         Process   := Get_Process (View);
         Model     := -Get_Model (View.Breakpoint_List);
         Selection := Get_Selection_Index (View);

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
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access View_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Breakpoint_Editor :=
        Breakpoint_Editor
          (Breakpoints_MDI_Views.Retrieve_View (Get_Kernel (Context.Context)));
      Process   : Visual_Debugger;
      Selection : Integer;
   begin
      if View /= null then
         Process   := Get_Process (View);
         Selection := Get_Selection_Index (View);
         if Selection /= -1 then
            --  ??? We should not be changing the current location, just
            --  showing the editor
            Process.Editor_Text.Set_Current_File_And_Line
              (File => Process.Breakpoints (Selection).File,
               Line => Process.Breakpoints (Selection).Line);
         end if;
      end if;
      return Success;
   end Execute;

   ---------------------
   -- On_Type_Changed --
   ---------------------

   procedure On_Type_Changed (W : access GObject_Record'Class) is
      Self : constant Properties_Editor := Properties_Editor (W);
      T    : constant Breakpoint_Type :=
        Breakpoint_Type'Val (Get_Active (Self.Breakpoint_Type));
   begin
      Self.Location_Box.Set_Sensitive (T = Break_On_Source_Loc);
      Self.Location_Box.Set_Visible (T = Break_On_Source_Loc);

      Self.Subprogram_Box.Set_Sensitive (T = Break_On_Subprogram);
      Self.Subprogram_Box.Set_Visible (T = Break_On_Subprogram);

      Self.Address_Box.Set_Sensitive (T = Break_At_Address);
      Self.Address_Box.Set_Visible (T = Break_At_Address);

      Self.Regexp_Box.Set_Sensitive (T = Break_On_Regexp);
      Self.Regexp_Box.Set_Visible (T = Break_On_Regexp);

      Self.Variable_Box.Set_Sensitive (T = Break_On_Variable);
      Self.Variable_Box.Set_Visible (T = Break_On_Variable);

      Self.Exception_Box.Set_Sensitive (T = Break_On_Exception);
      Self.Exception_Box.Set_Visible (T = Break_On_Exception);
   end On_Type_Changed;

end GVD.Breakpoints;
