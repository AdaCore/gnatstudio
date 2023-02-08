------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

pragma Warnings (Off);

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Strings;               use GNAT.Strings;

with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gtk.Adjustment;             use Gtk.Adjustment;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Toggle;   use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Gesture_Long_Press;     use Gtk.Gesture_Long_Press;
with Gtk.Gesture_Multi_Press;    use Gtk.Gesture_Multi_Press;
with Gtk.Label;                  use Gtk.Label;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Size_Group;             use Gtk.Size_Group;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Spin_Button;            use Gtk.Spin_Button;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Radio_Button;           use Gtk.Radio_Button;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Multiline_Entry;     use Gtkada.Multiline_Entry;

with Basic_Types;                use Basic_Types;

with GPS.Debuggers;              use GPS.Debuggers;
with GPS.Editors;                use GPS.Editors;
with GPS.Main_Window;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Markers;                use GPS.Markers;
with GPS.Kernel.Actions;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;

with DAP.Types;                  use DAP.Types;
with DAP.Modules.Breakpoints;    use DAP.Modules.Breakpoints;
with DAP.Modules.Persistent_Breakpoints;
use DAP.Modules.Persistent_Breakpoints;
with DAP.Clients;                use DAP.Clients;
with DAP.Module;
with DAP.Utils;

with GUI_Utils;                  use GUI_Utils;

package body DAP.Views.Breakpoints is

   Me : constant Trace_Handle := Create ("GPS.DAP.BREAKPOINTS_VIEW");

   Col_Num         : constant Gint := 0;
   Col_Enb         : constant Gint := 1;
   Col_Type        : constant Gint := 2;
   Col_Disp        : constant Gint := 3;
   Col_File        : constant Gint := 4;
   Col_Line        : constant Gint := 5;
   Col_Exception   : constant Gint := 6;
   Col_Subprogs    : constant Gint := 7;
   Col_Address     : constant Gint := 8;
   Col_Activatable : constant Gint := 9;
   Col_Executable  : constant Gint := 10;

   Column_Types : constant Glib.GType_Array (0 .. 10) :=
     (Guint (Col_Enb)         => GType_Boolean,
      Guint (Col_Activatable) => GType_Boolean,
      others                  => GType_String);

   Column_Names : constant GNAT.Strings.String_List (1 .. 11) :=
     (new String'("Num"),
      new String'("Enb"),
      new String'("Type"),
      new String'("Disp"),
      new String'("File/Variable"),
      new String'("Line"),
      new String'("Exception"),
      new String'("Subprograms"),
      new String'("Address"),
      new String'("Activatable"),
      new String'("Executable"));

   type Breakpoint_Type is
     (Break_On_Source_Loc,
      Break_On_Subprogram);
   --  The various types of breakpoints

   type Breakpoint_View_Record is new View_Record with
      record
         List                 : Gtk_Tree_View;
         Multipress           : Gtk_Gesture_Multi_Press;
         Longpress            : Gtk_Gesture_Long_Press;
         Activatable          : Boolean := True;
         Prevent_Bp_Selection : Boolean := False;
      end record;
   type Breakpoint_View is access all Breakpoint_View_Record'Class;

   function Initialize
     (Self : access Breakpoint_View_Record'Class) return Gtk_Widget;
   --  Internal initialization function
   --  Returns the focus child

   overriding procedure On_Process_Terminated
     (View : not null access Breakpoint_View_Record);

   overriding procedure On_Status_Changed
     (Self   : not null access Breakpoint_View_Record;
      Status : GPS.Debuggers.Debugger_State);

   overriding procedure On_Location_Changed
     (Self : not null access Breakpoint_View_Record);

   overriding procedure Update (View : not null access Breakpoint_View_Record);

   procedure Get_Selected_Breakpoints_Or_Set_State
     (View    : not null access Breakpoint_View_Record'Class;
      Is_Set  : Boolean;
      State   : Boolean;
      Id_List : in out Breakpoint_Identifier_Lists.List);

   procedure Show_Selected_Breakpoint_Details
     (View : not null access Breakpoint_View_Record'Class);

   procedure Show_Selected_Breakpoint_In_Editor
     (View : not null access Breakpoint_View_Record'Class);

   procedure On_Longpress
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble);

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble);

   function Get_Selection
     (View : access Breakpoint_View_Record'Class) return Breakpoint_Data;
   --  Return information on the currently selected breakpoint.

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Breakpoint_View_Record'Class;
   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Breakpoint_View_Record'Class := null);
   --  Store or retrieve the view from the process

   procedure Recompute_Filters
     (Self : access Glib.Object.GObject_Record'Class);

   procedure On_Breakpoint_State_Toggled
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Glib.UTF8_String);
   --  Called when the state of a brekpoint is toggled.

   package Breakpoints_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Breakpoints",
      View_Name                       => "Breakpoints",
      Formal_View_Record              => Breakpoint_View_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => True,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Local_Toolbar                   => True,
      Areas                           => Gtkada.MDI.Sides_Only,
      Position                        => Position_Automatic,
      Initialize                      => Initialize);
   package Simple_Views is new DAP.Views.Simple_Views
     (Works_Without_Debugger => True,
      Formal_Views           => Breakpoints_MDI_Views,
      Formal_View_Record     => Breakpoint_View_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Get_View               => Get_View,
      Set_View               => Set_View);

   --  Filters --

   type No_View_Filter is new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access No_View_Filter;
      Context : Selection_Context) return Boolean;

   type Dummy_Filter is new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Dummy_Filter;
      Context : Selection_Context) return Boolean;

   type Breakpoint_Single_Selection is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Breakpoint_Single_Selection;
      Context : Selection_Context) return Boolean;
   --  True if only one row is selected.

   --  Hooks --

   type On_Breakpoints_Changed is new Debugger_Hooks_Function
      with null record;
   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class);
   --  Hook for "debugger_breakpoints_changed"

   -- Properties_Editor --

   type Properties_Editor_Record is new Gtk_Dialog_Record with record
      Kernel               : access Kernel_Handle_Record'Class;

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
      Command_Descr        : Gtkada_Multiline_Entry;

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
     (Self   : not null access Properties_Editor_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Create the breakpoint properties editor

   procedure Fill
     (Self : not null access Properties_Editor_Record'Class;
      Br   : Breakpoint_Data);
   --  Show the information for the given breakpoint in the editor

   procedure Apply
     (Self : not null access Properties_Editor_Record'Class;
      Br   : in out Breakpoint_Data);
   --  Apply the settings to the given breakpoint

   procedure On_Type_Changed (W : access GObject_Record'Class);
   --  Callbacks for the various buttons

   -- Commands --

   type Remove_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove the selected breakpoint

   type Clear_Breakpoints_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Clear_Breakpoints_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove all breakpoints

   type Set_Breakpoints_State_Command (Is_Enabled : Boolean) is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Breakpoints_State_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Set the state of the selected breakpoints to Is_Enabled

   type Add_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Create a new breakpoint

   type Advanced_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Advanced_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit the advanced properties of the selected breakpoint

   type Dummy_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Dummy_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
     is (Success);

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Self : not null access Properties_Editor_Record'Class;
      Br   : in out Breakpoint_Data)
   is
      use type Generic_Views.Abstract_View_Access;

      Modified       : Boolean := False;
      Start, The_End : Gtk_Text_Iter;
      C              : Integer;
      T              : constant Breakpoint_Type :=
        Breakpoint_Type'Val (Get_Active (Self.Breakpoint_Type));
      Temporary      : constant Boolean := Self.Temporary.Get_Active;
      Num            : Breakpoint_Identifier := Br.Num;

   begin
      --  Create a new breakpoint if needed

      if Num = 0 then
         case T is
         when Break_On_Source_Loc =>
            declare
               File : constant Filesystem_String := +Get_Text (Self.File_Name);
            begin
               Break_Source
                 (Self.Kernel,
                  File      => Create_From_Base (File),
                  Line      =>
                    Editable_Line_Type'Value (Self.Line_Spin.Get_Text),
                  Temporary => Temporary);
            end;

         when Break_On_Subprogram =>
            Break_Subprogram
              (Self.Kernel,
               Subprogram => Self.Subprogram_Combo.Get_Active_Text,
               Temporary  => Temporary);

         end case;
      end if;

      if Modified
        and then DAP.Module.Get_Breakpoints_View /= null
      then
         DAP.Views.View_Access (DAP.Module.Get_Breakpoints_View).Update;
      end if;
   end Apply;

   -------------------------------------------
   -- Get_Selected_Breakpoints_Or_Set_State --
   -------------------------------------------

   procedure Get_Selected_Breakpoints_Or_Set_State
     (View    : not null access Breakpoint_View_Record'Class;
      Is_Set  : Boolean;
      State   : Boolean;
      Id_List : in out Breakpoint_Identifier_Lists.List)
   is
      Selection   : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Path        : Gtk_Tree_Path;
      Iter        : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;
      Store_Model : Gtk_Tree_Store;
      Path_List   : Gtk_Tree_Path_List.Glist;
      G_Iter      : Gtk_Tree_Path_List.Glist;

      use type Gtk_Tree_Path_List.Glist;
   begin
      Selection := View.List.Get_Selection;
      Selection.Get_Selected_Rows (Model, Path_List);

      if not (Model = Null_Gtk_Tree_Model
              or else Path_List = Gtk_Tree_Path_List.Null_List)
      then
         --  Store_Model is needed to modify the Breakpoint Model
         Store_Model := -Get_Model (View.List);
         G_Iter := Gtk_Tree_Path_List.Last (Path_List);

         while G_Iter /= Gtk_Tree_Path_List.Null_List loop
            Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
            if Path /= Null_Gtk_Tree_Path then
               Iter := Get_Iter (Model, Path);
            end if;

            if Iter /= Null_Iter then
               if Is_Set then
                  Store_Model.Set (Iter, Col_Enb, State);
               else
                  Id_List.Append (Breakpoint_Identifier'Value
                                  (Get_String (Model, Iter, Col_Num)));
               end if;
            end if;

            G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
         end loop;
      end if;
      Free_Path_List (Path_List);
   end Get_Selected_Breakpoints_Or_Set_State;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Breakpoint_View_Record'Class is
   begin
      return Breakpoint_View (DAP.Module.Get_Breakpoints_View);
   end Get_View;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Breakpoint_View_Record'Class) return Gtk_Widget
   is
      Main_Vbox : Gtk_Box;
      Scroll    : Gtk_Scrolled_Window;
   begin
      Trace (Me, "Initialize");
      Gtk.Box.Initialize_Hbox (Self);

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scroll);

      Gtk_New_Vbox (Main_Vbox);
      Scroll.Add_With_Viewport (Main_Vbox);

      ----------
      --  List of breakpoints
      ----------

      Self.List := Create_Tree_View
        (Column_Types, Column_Names, Sortable_Columns => False);
      Self.List.Get_Selection.Set_Mode (Selection_Multiple);
      Self.List.Get_Selection.On_Changed
        (Recompute_Filters'Access, Self);
      Self.List.Set_Search_Column (Col_File);
      Main_Vbox.Pack_Start (Self.List);

      Self.List.Get_Column (Col_Activatable).Set_Visible (False);

      declare
         List : Cell_Renderer_List.Glist;
      begin
         List := Self.List.Get_Column (Col_Enb).Get_Cells;
         Self.List.Get_Column (Col_Enb).Add_Attribute
           (Cell_Renderer_List.Get_Data (List),
            "activatable",
            Col_Activatable);
         Gtk_Cell_Renderer_Toggle
           (Cell_Renderer_List.Get_Data (List)).On_Toggled
             (Call  => On_Breakpoint_State_Toggled'Access,
              Slot  => Self);
         Cell_Renderer_List.Free (List);
      end;

      Gtk_New (Self.Multipress, Widget => Self.List);
      Self.Multipress.On_Pressed (On_Multipress'Access, Slot => Self);
      Self.Multipress.Watch (Self);

      Gtk_New (Self.Longpress, Widget => Self.List);
      Self.Longpress.On_Pressed (On_Longpress'Access, Slot => Self);
      Self.Longpress.Watch (Self);

      Debugger_Breakpoints_Changed_Hook.Add
         (new On_Breakpoints_Changed, Watch => Self);

      --  Initial display
      Update (Self);

      return Gtk_Widget (Self.List);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Properties_Editor_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      Button  : Gtk_Button;
      Frame   : Gtk_Frame;
      Label   : Gtk_Label;
      Scroll  : Gtk_Scrolled_Window;
      Vbox9   : Gtk_Vbox;
      Details : Gtk_Vbox;
      Hbox    : Gtk_Box;
      Size    : Gtk_Size_Group;
      Adj     : Gtk_Adjustment;
      M       : Gtk_List_Store;
      Dummy   : Gtk_Widget;
   begin
      Gtk.Dialog.Initialize
        (Self,
         "Breakpoint editor",
         Parent => Kernel.Get_Main_Window,
         Flags  => Destroy_With_Parent);

      GPS.Main_Window.Set_Default_Size_From_History
        (Self, "breakpoints", Kernel, 600, 500);

      Self.Kernel := Kernel;

      ------------
      --  Choosing the type of the breakpoint
      ------------

      Gtk_New (Self.Breakpoint_Type);
      for T in Breakpoint_Type loop
         case T is
            when Break_On_Source_Loc =>
               Self.Breakpoint_Type.Append_Text ("break on source location");
            when Break_On_Subprogram =>
               Self.Breakpoint_Type.Append_Text ("break on subprogram");
         end case;
      end loop;

      Self.Breakpoint_Type.Set_Active
        (Breakpoint_Type'Pos (Break_On_Source_Loc));

      Self.Breakpoint_Type.On_Changed (On_Type_Changed'Access, Self);

      Gtk_New (Frame);
      Frame.Set_Label_Widget (Self.Breakpoint_Type);
      Self.Get_Content_Area.Pack_Start (Frame, Expand => True, Fill => True);

      Gtk_New_Vbox (Details);
      Frame.Add (Details);

      ------------
      --  Break on source location
      ------------

      Gtk_New_Vbox (Self.Location_Box);
      Details.Pack_Start (Self.Location_Box, Expand => False, Fill => True);

      Gtk_New (Size);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Location_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, "File:");
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False, 0);
      Size.Add_Widget (Label);

      Gtk_New (Self.File_Name);
      Self.File_Name.Set_Editable (True);
      Self.File_Name.Set_Activates_Default (True);
      Hbox.Pack_Start (Self.File_Name, True, True, 0);

      Gtk_New_Hbox (Hbox, Spacing => 15);
      Self.Location_Box.Pack_Start (Hbox, False, False);

      Gtk_New (Label, "Line:");
      Label.Set_Alignment (1.0, 0.5);
      Hbox.Pack_Start (Label, False, False, 0);
      Size.Add_Widget (Label);

      Gtk_New (Self.Line_Spin, Min => 1.0, Max => 1.0e+08, Step => 1.0);
      Self.Line_Spin.Set_Numeric (True);
      Self.Line_Spin.Set_Value (1.0);
      Self.Line_Spin.Set_Activates_Default (True);
      Hbox.Pack_Start (Self.Line_Spin, True, True, 0);

      ------------
      --   Break on subprogram
      ------------

      Gtk_New_Vbox (Self.Subprogram_Box);
      Details.Pack_Start (Self.Subprogram_Box, Expand => True, Fill => True);

      Gtk_New_With_Entry (Self.Subprogram_Combo);
      Self.Subprogram_Combo.Append_Text ("");
      Self.Subprogram_Box.Pack_Start (Self.Subprogram_Combo, False, False);

      -------------
      --  A temporary breakpoint ?
      -------------

      Gtk_New (Self.Temporary, "Temporary breakpoint");
      Self.Temporary.Set_Active (False);
      Details.Pack_Start (Self.Temporary, False, False, 5);

      --------------
      --  Set proper visibility
      --------------

      Self.Show_All;
      Self.Location_Box.Set_No_Show_All (True);
      Self.Subprogram_Box.Set_No_Show_All (True);

      --------------
      --  Fill information
      --------------

      --  Reinitialize the contents of the file name entry
      if DAP.Module.Get_Current_Debugger /= null then
         Set_Text
           (Self.File_Name,
            +Relative_Path
              (DAP.Module.Get_Current_Debugger.Current_File, Get_Current_Dir));
      end if;

      On_Type_Changed (Self);

      ----------------
      --  Action buttons
      ----------------

      Dummy := Self.Add_Button (Stock_Ok, Gtk_Response_Apply);
      Dummy.Grab_Default;
      Dummy := Self.Add_Button (Stock_Cancel, Gtk_Response_Cancel);
      Self.Set_Default_Response (Gtk_Response_Apply);
   end Initialize;

   ---------------------------------
   -- On_Breakpoint_State_Toggled --
   ---------------------------------

   procedure On_Breakpoint_State_Toggled
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Glib.UTF8_String)
   is
      View   : constant Breakpoint_View := Breakpoint_View (Self);
      Model  : constant Gtk_Tree_Store  := -Get_Model (View.List);
      Iter   : constant Gtk_Tree_Iter   := Model.Get_Iter_From_String (Path);
      Vector : Breakpoint_Identifier_Lists.List;
   begin
      if Iter /= Null_Iter then
         Vector.Append
           (DAP.Types.Breakpoint_Identifier'Value
              (Get_String (Model, Iter, Col_Num)));
         Set_Breakpoints_State
           (View.Kernel,
            List  => Vector,
            State => Model.Get_Boolean (Iter, Col_Enb));
      end if;
   end On_Breakpoint_State_Toggled;

   -------------------------
   -- On_Location_Changed --
   -------------------------

   overriding procedure On_Location_Changed
     (Self : not null access Breakpoint_View_Record)
   is
      Client : DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Model  : Gtk_Tree_Store;
      Iter   : Gtk_Tree_Iter;
      Id     : Breakpoint_Identifier := 0;
   begin
      if Self.Prevent_Bp_Selection
        or else (Client /= null
                 and then Client.Current_File = No_File)
      then
         return;
      end if;

      if Client /= null then
         for Data of Client.Get_Breakpoints loop
            for L of Data.Locations loop
               if Get_File (L.Marker) = Client.Current_File
                 and then Natural (Get_Line (L.Marker)) = Client.Current_Line
               then
                  Id := Data.Num;
                  exit;
               end if;
            end loop;
         end loop;
      end if;

      if Id /= 0 then
         Model := -Get_Model (Self.List);
         Iter  := Model.Get_Iter_First;

         while Iter /= Null_Iter loop
            if Breakpoint_Identifier'Value
              (Model.Get_String (Iter, Col_Num)) = Id
            then
               Self.List.Get_Selection.Unselect_All;
               Self.List.Get_Selection.Select_Iter (Iter);
               return;
            end if;

            Model.Next (Iter);
         end loop;
      end if;
   end On_Location_Changed;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Breakpoint_View_Record) is
   begin
      if DAP.Module.Count_Running_Debuggers < 2 then
         --  The last debugger is exiting
         --  Show the persistent breakpoints
         Update (View);
      else
         Gtk.Tree_Store.Clear (-Gtk.Tree_View.Get_Model (View.List));
      end if;
   end On_Process_Terminated;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (Self   : not null access Breakpoint_View_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      Model : constant Gtk_Tree_Store := -Get_Model (Self.List);
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Status = Debug_Available then
         Self.Activatable := True;
      else
         Self.Activatable := False;
      end if;

      Iter := Model.Get_Iter_First;
      while Iter /= Null_Iter loop
         Model.Set_Value
           (Iter, Col_Activatable, As_Boolean (Self.Activatable));
         Model.Next (Iter);
      end loop;
   end On_Status_Changed;

   -----------------------
   -- Recompute_Filters --
   -----------------------

   procedure Recompute_Filters (Self : access Glib.Object.GObject_Record'Class)
   is
      View   : constant Breakpoint_View := Breakpoint_View (Self);
      Kernel : constant Kernel_Handle   := View.Kernel;
   begin
      --  Must refresh the context to update the value of the Selection Filter
      Kernel.Refresh_Context;
   end Recompute_Filters;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Breakpoint_View_Record'Class := null)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if DAP.Module.Get_Breakpoints_View /= null then
         Breakpoint_View
           (DAP.Module.Get_Breakpoints_View).On_Process_Terminated;
      end if;

      DAP.Module.Set_Breakpoints_View
        (Generic_Views.Abstract_View_Access (View));
   end Set_View;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Breakpoint_View_Record)
   is
      Client  : DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Model   : constant Gtk_Tree_Store := -Get_Model (View.List);
      Iter    : Gtk_Tree_Iter;
      Values  : Glib.Values.GValue_Array (1 .. 11);
      Columns : Columns_Array (Values'Range);
      Last    : Gint;

      ----------
      -- Fill --
      ----------

      procedure Fill (Data : Breakpoint_Data);

      procedure Fill (Data : Breakpoint_Data) is
      begin
         Append (Model, Iter, Null_Iter);

         Columns (1 .. 5) :=
           (Col_Num, Col_Enb, Col_Activatable, Col_Type, Col_Disp);
         Values  (1 .. 3) :=
           (1 => As_String (
                  if Data.Num = Breakpoint_Identifier'Last
                  then "0"
                  else Breakpoint_Identifier'Image (Data.Num)),
            2 => As_Boolean (Data.State = Enabled),
            3 => As_Boolean (View.Activatable));
         Last := 5;

         Glib.Values.Init_Set_String (Values (4), "break");
         Glib.Values.Init_Set_String
           (Values (5), To_Lower (Data.Disposition'Img));

         if not Data.Locations.Is_Empty then
            if Last < 6 then
               Last := Last + 1;
               Columns (Last) := Col_File;
               Glib.Values.Init
                 (Values (Last), Column_Types (Guint (Col_File)));
            end if;
            Glib.Values.Set_String
              (Values (Last), Escape_Text
               (+Base_Name (Get_File (Get_Location (Data)))));

            Last := Last + 1;
            Columns (Last) := Col_Line;
            Glib.Values.Init_Set_String
              (Values (Last), Get_Line (Get_Location (Data))'Img);

            if Data.Locations.First_Element.Address /= Invalid_Address then
               Last := Last + 1;
               Columns (Last) := Col_Address;
               Glib.Values.Init_Set_String
                 (Values (Last),
                  Escape_Text
                    (Address_To_String
                         (Data.Locations.First_Element.Address)));
            end if;
         end if;

         if Data.Subprogram /= "" then
            Last := Last + 1;
            Columns (Last) := Col_Subprogs;
            Glib.Values.Init_Set_String
              (Values (Last), Escape_Text (To_String (Data.Subprogram)));
         end if;

         if Data.Executable /= Null_Unbounded_String then
            Last := Last + 1;
            Columns (Last) := Col_Executable;
            Glib.Values.Init_Set_String
              (Values (Last),
               Escape_Text (To_String (Data.Executable)));
         end if;

         Set_And_Clear (Model, Iter, Columns (1 .. Last), Values (1 .. Last));
      end Fill;

   begin
      Trace (Me, "Update" & DAP.Modules.Persistent_Breakpoints.
           Get_Persistent_Breakpoints.Length'Img);

      Clear (Model);

      if Client = null then
         for Data of DAP.Modules.Persistent_Breakpoints.
           Get_Persistent_Breakpoints
         loop
            Fill (Data);
         end loop;

      else
         for Data of Client.Get_Breakpoints loop
            Fill (Data);
         end loop;

         View.On_Location_Changed;
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
      pragma Unreferenced (Self);
      View : Breakpoint_View;

   begin
      if Debugger /= null
        and then Positive (Debugger.Get_Num) /=
        DAP.Module.Get_Current_Debugger.Id
      then
         return;
      end if;

      View := Breakpoint_View
        (Breakpoints_MDI_Views.Retrieve_View
           (Kernel, Visible_Only => True));

      if View /= null then
         Update (View);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Advanced_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Breakpoint_View :=
        Breakpoint_View
          (Breakpoints_MDI_Views.Retrieve_View
             (Get_Kernel (Context.Context),
              Visible_Only => True));
   begin
      if View /= null then
         Show_Selected_Breakpoint_Details (View);
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle   := Get_Kernel (Context.Context);
      View   : constant Breakpoint_View :=
        Breakpoint_View
           (Breakpoints_MDI_Views.Retrieve_View
                (Kernel,
                 Visible_Only => True));
      Id_List : Breakpoint_Identifier_Lists.List;
   begin
      --  Get the list of selected breakpoints
      Get_Selected_Breakpoints_Or_Set_State (View    => View,
                                             Is_Set  => False,
                                             State   => False,
                                             Id_List => Id_List);
      --  Put them in numerical order
      Breakpoint_Identifier_Lists.Reverse_Elements (Id_List);
      Delete_Multiple_Breakpoints (Kernel, Id_List);
      Breakpoint_Identifier_Lists.Clear (Id_List);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Clear_Breakpoints_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle   := Get_Kernel (Context.Context);
      View   : constant Breakpoint_View :=
        Breakpoint_View
           (Breakpoints_MDI_Views.Retrieve_View
                (Kernel,
                 Visible_Only => True));
   begin
      if View /= null then
         Clear_All_Breakpoints (Kernel);
         Gtk.Tree_Store.Clear (-Get_Model (View.List));
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Set_Breakpoints_State_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle   := Get_Kernel (Context.Context);
      View    : constant Breakpoint_View :=
        Breakpoint_View
           (Breakpoints_MDI_Views.Retrieve_View
                (Kernel,
                 Visible_Only => True));
      Id_List : Breakpoint_Identifier_Lists.List;
   begin
      if View = null then
         return Commands.Failure;
      end if;

      --  Get the list of selected breakpoints
      Get_Selected_Breakpoints_Or_Set_State (View    => View,
                                             Is_Set  => False,
                                             State   => Command.Is_Enabled,
                                             Id_List => Id_List);

      --  Put them in numerical order
      Breakpoint_Identifier_Lists.Reverse_Elements (Id_List);
      Set_Breakpoints_State (View.Kernel, Id_List, Command.Is_Enabled);

      Breakpoint_Identifier_Lists.Clear (Id_List);

      --  Need to modify the toggle buttons in the model
      Get_Selected_Breakpoints_Or_Set_State (View    => View,
                                             Is_Set  => True,
                                             State   => Command.Is_Enabled,
                                             Id_List => Id_List);

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Breakpoint_View :=
        Breakpoint_View
          (Breakpoints_MDI_Views.Retrieve_View
             (Get_Kernel (Context.Context),
              Visible_Only => True));
      Props : Properties_Editor;
      Br    : Breakpoint_Data := (Num => 0, others => <>);
   begin
      if View /= null then
         Props := new Properties_Editor_Record;
         Initialize (Props, View.Kernel);
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

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access No_View_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use type DAP.Clients.DAP_Client_Access;
   begin
      return DAP.Module.Get_Current_Debugger = null
        or else Get_View (DAP.Module.Get_Current_Debugger) = null;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Breakpoint_Single_Selection;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      View : constant Breakpoint_View :=
        Breakpoint_View
          (Breakpoints_MDI_Views.Retrieve_View
             (Get_Kernel (Context),
              Visible_Only => True));
      Res  : Boolean                    := False;
   begin
      if View /= null then
         declare
            Selection : constant Gtk_Tree_Selection :=
              Get_Selection (View.List);
         begin
            Res := Selection.Count_Selected_Rows = 1;
         end;
      end if;
      return Res;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Dummy_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return False;
   end Filter_Matches_Primitive;

   ------------------
   -- On_Longpress --
   ------------------

   procedure On_Longpress
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble)
   is
      pragma Unreferenced (X, Y);
      View : constant Breakpoint_View := Breakpoint_View (Self);
   begin
      Show_Selected_Breakpoint_Details (View);
      View.Longpress.Set_State (Event_Sequence_Claimed);
   end On_Longpress;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble)
   is
      pragma Unreferenced (X, Y);
      View : constant Breakpoint_View := Breakpoint_View (Self);
   begin
      if N_Press = 2 then
         Show_Selected_Breakpoint_In_Editor (View);
         View.Multipress.Set_State (Event_Sequence_Claimed);
      end if;
   end On_Multipress;

   --------------------------------------
   -- Show_Selected_Breakpoint_Details --
   --------------------------------------

   procedure Show_Selected_Breakpoint_Details
     (View : not null access Breakpoint_View_Record'Class)
   is
      Current : Breakpoint_Data;
      Props   : Properties_Editor;
   begin
      Current := Get_Selection (View);
      if Current /= Empty_Breakpoint_Data then
         Props := new Properties_Editor_Record;
         Initialize (Props, View.Kernel);
         Fill (Props, Current);

         if Props.Run = Gtk_Response_Apply then
            Apply (Props, Current);
         end if;

         Props.Destroy;
      end if;
   end Show_Selected_Breakpoint_Details;

   ----------------------------------------
   -- Show_Selected_Breakpoint_In_Editor --
   ----------------------------------------

   procedure Show_Selected_Breakpoint_In_Editor
     (View : not null access Breakpoint_View_Record'Class)
   is
      Selection : Breakpoint_Data;
   begin
      Selection := Get_Selection (View);
      if Selection /= Empty_Breakpoint_Data then
         View.Prevent_Bp_Selection := True;
         DAP.Utils.Goto_Location
           (Kernel    => View.Kernel,
            File      => Get_File (Selection.Locations.First_Element.Marker),
            Line      => Natural
              (Get_Line (Selection.Locations.First_Element.Marker)));
         View.Prevent_Bp_Selection := False;
      end if;
   end Show_Selected_Breakpoint_In_Editor;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (View : access Breakpoint_View_Record'Class) return Breakpoint_Data
   is
      Iter      : Gtk_Tree_Iter;
      The_Model : Gtk_Tree_Model;
      List      : Gtk_Tree_Path_List.Glist;
      Path      : Gtk_Tree_Path;
      Selection : constant Gtk_Tree_Selection := Get_Selection (View.List);
   begin
      if Selection.Count_Selected_Rows = 1 then
         Get_Selected_Rows (Selection, The_Model, List);
         Path := Gtk_Tree_Path
           (Gtk_Tree_Path_List.Get_Data (Gtk_Tree_Path_List.First (List)));
         Iter := Get_Iter (The_Model, Path);
         Free_Path_List (List);

         if Iter /= Null_Iter then
            return DAP.Module.Get_Breakpoint_From_Id
              (Breakpoint_Identifier'Value
                 (Get_String (The_Model, Iter, Col_Num)));
         end if;
      end if;

      return Empty_Breakpoint_Data;
   end Get_Selection;

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
   end On_Type_Changed;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Self : not null access Properties_Editor_Record'Class;
      Br   : Breakpoint_Data)
   is
      Start, The_End : Gtk_Text_Iter;
      Buffer         : Gtk_Text_Buffer;
   begin
      --  When editing an existing breakpoint, we can't change its type
      if Br.Num /= 0 then
         if not Br.Locations.Is_Empty then
            Set_Text
              (Self.File_Name,
               +Base_Name (Get_File (Br.Locations.First_Element.Marker)));
            Set_Value
              (Self.Line_Spin,
               Grange_Float (Get_Line (Br.Locations.First_Element.Marker)));
         end if;

         if Br.Subprogram /= "" then
            Self.Breakpoint_Type.Set_Active
              (Breakpoint_Type'Pos (Break_On_Subprogram));

            Add_Unique_Combo_Entry
              (Self.Subprogram_Combo, To_String (Br.Subprogram), True);
         end if;

         Self.Breakpoint_Type.Set_Sensitive (False);
         Self.Temporary.Set_Sensitive (False);
         Self.File_Name.Set_Sensitive (False);
         Self.Line_Spin.Set_Sensitive (False);
         Self.Subprogram_Combo.Set_Sensitive (False);
      end if;
   end Fill;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      No_View            : Action_Filter := new No_View_Filter;
      Selection_Filter   : constant Action_Filter :=
        new Breakpoint_Single_Selection;

      No_Or_Ready_Filter : Action_Filter;
      Dummy              : Action_Filter;
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open breakpoints editor",
         Description => "Open the Breakpoints Editor for the debugger",
         Filter      => No_View);

      No_Or_Ready_Filter := Kernel.Lookup_Filter ("No debugger or ready");

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug delete breakpoint", new Remove_Breakpoint_Command,
         "Delete the selected breakpoints"
           & " (from the Breakpoints view)",
         Icon_Name => "gps-remove-symbolic",
         Category  => "Debug",
         Filter    => No_Or_Ready_Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug clear breakpoints", new Clear_Breakpoints_Command,
         "Delete all existing breakpoints",
         Icon_Name => "gps-clear-symbolic",
         Category  => "Debug",
         Filter    => No_Or_Ready_Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug enable breakpoints",
         new Set_Breakpoints_State_Command (True),
         "Enable the selected breakpoints",
         Icon_Name => "gps-syntax-check-symbolic",
         Category  => "Debug",
         Filter    => No_Or_Ready_Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug disable breakpoints",
         new Set_Breakpoints_State_Command (False),
         "Disable the selected breakpoints",
         Icon_Name => "gps-stop-symbolic",
         Category  => "Debug",
         Filter    => No_Or_Ready_Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug create breakpoint", new Add_Command,
         "Create a new breakpoint, from the Breakpoints view",
         Icon_Name => "gps-add-symbolic",
         Category  => "Debug",
         Filter    => No_Or_Ready_Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug edit breakpoint", new Advanced_Command,
         "Edit the advanced properties of the selected breakpoint"
           & " like its condition, repeat count,..."
           & " (from the Breakpoints view)",
         Icon_Name => "gps-settings-symbolic",
         Category  => "Debug",
         Filter    => No_Or_Ready_Filter and Selection_Filter);

      --  Not implemented yet
      Dummy := new Dummy_Filter;
      Register_Filter (Kernel, Dummy, "Dummy");

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug view breakpoint", new Dummy_Command,
         "View the source editor containing the selected breakpoint"
           & " (from the Breakpoints view)",
         Icon_Name => "gps-goto-symbolic",
         Category  => "Debug",
         Filter    => Dummy);
   end Register_Module;

end DAP.Views.Breakpoints;
