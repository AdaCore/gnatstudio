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

with Gdk.RGBA;                   use Gdk.RGBA;
with Gtk.Adjustment;             use Gtk.Adjustment;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;   use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Combo_Box_Text;         use Gtk.Combo_Box_Text;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Gesture_Long_Press;     use Gtk.Gesture_Long_Press;
with Gtk.Gesture_Multi_Press;    use Gtk.Gesture_Multi_Press;
with Gtk.Label;                  use Gtk.Label;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Spin_Button;            use Gtk.Spin_Button;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Radio_Button;           use Gtk.Radio_Button;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Multiline_Entry;     use Gtkada.Multiline_Entry;

with VSS.Regular_Expressions;
with VSS.Strings.Conversions;
with VSS.String_Vectors;

with Basic_Types;                use Basic_Types;

with Dialog_Utils;               use Dialog_Utils;
with GPS.Debuggers;              use GPS.Debuggers;
with GPS.Default_Styles;
with GPS.Editors;                use GPS.Editors;
with GPS.Main_Window;
with GPS.Markers;                use GPS.Markers;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Actions;
with GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;   use GPS.Kernel.Style_Manager;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;

with DAP.Types;                  use DAP.Types;
with DAP.Module.Breakpoints;     use DAP.Module.Breakpoints;
with DAP.Types.Breakpoints;      use DAP.Types.Breakpoints;
with DAP.Clients;                use DAP.Clients;
with DAP.Clients.Breakpoint_Managers;
with DAP.Clients.Stack_Trace;    use DAP.Clients.Stack_Trace;
with DAP.Module;
with DAP.Tools;                  use DAP.Tools;
with DAP.Utils;                  use DAP.Utils;

with GUI_Utils;                  use GUI_Utils;

with DAP.Requests.Evaluate;

package body DAP.Views.Breakpoints is

   Me : constant Trace_Handle := Create ("GPS.DAP.BREAKPOINTS_VIEW");

   Col_Enb         : constant Gint := 0;
   Col_Type        : constant Gint := 1;
   Col_Disp        : constant Gint := 2;
   Col_Num         : constant Gint := 3;
   Col_File        : constant Gint := 4;
   Col_Line        : constant Gint := 5;
   Col_Exception   : constant Gint := 6;
   Col_Subprogs    : constant Gint := 7;
   Col_Address     : constant Gint := 8;
   Col_Activatable : constant Gint := 9;
   Col_Fg_Color    : constant Gint := 10;

   Column_Types : constant Glib.GType_Array (0 .. 10) :=
     (Guint (Col_Enb)         => GType_Boolean,
      Guint (Col_Activatable) => GType_Boolean,
      Guint (Col_Fg_Color)    => Gdk.RGBA.Get_Type,
      others                  => GType_String);

   Column_Names : constant GNAT.Strings.String_List (1 .. 10) :=
     (new String'("Enb"),
      new String'("Type"),
      new String'("Disp"),
      new String'("Num"),
      new String'("File/Variable"),
      new String'("Line"),
      new String'("Exception"),
      new String'("Subprograms"),
      new String'("Address"),
      new String'("Activatable"));

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

   function Get_Iter_For_Index
     (Self  : not null access Breakpoint_View_Record'Class;
      Index : Positive) return Gtk_Tree_Iter;
   --  Return the Iter for the given breakpoint index

   function Get_Index_For_Breakpoint_Id
     (Self  : not null access Breakpoint_View_Record'Class;
      Num   : Breakpoint_Identifier) return Integer;
   --  Return the index corresponding to the given breakpoint's ID, if present
   --  in the Breakpoints view.
   --  Return -1 if there is no row for the given ID or if a null ID is passed.

   function Get_Breakpoint_For_Iter
     (Self : not null access Breakpoint_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Breakpoint_Data;
   --  Retrieve the breakpoint data associated with Iter.

   function Get_Index_For_Iter
     (Self : not null access Breakpoint_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Positive;
   --  Return the index of Iter in the view.

   procedure Update_Breakpoint
     (Self             : not null access Breakpoint_View_Record'Class;
      Data             : Breakpoint_Data;
      Create_If_Needed : Boolean := True);
   --  Update the row corresponding to the given breakpoint, searching it from
   --  the breakpoint's ID.
   --  If not found, create a row for it when Create_If_Needed is True.

   procedure Remove_Breakpoint
     (Self : not null access Breakpoint_View_Record'Class;
      Id   : Breakpoint_Identifier);
   --  Remove the breakpoint with the given Id from the Breakpoints' view.

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
      Indexes : in out Breakpoint_Index_Lists.List);
   --  Procedure used to factorize the code: It will loop through the list
   --  of selected rows and retrieve the indexes in a list if not Is_Set
   --  else set the State in the model.

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

   function Get_Selection_Index
     (View : access Breakpoint_View_Record'Class) return Integer;
   --  Return the index of the selected breakpoint or 0 when failing.

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
      Formal_MDI_Child       => GPS_MDI_Child_Record);

   --  Filters --

   type Breakpoint_Single_Selection is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Breakpoint_Single_Selection;
      Context : Selection_Context) return Boolean;
   --  True if only one row is selected.

   --  Hooks --

   type On_Breakpoint_Event (Event : Breakpoint_Event)
   is new Debugger_Breakpoint_Hook_Function with null record;
   overriding procedure Execute
      (Self     : On_Breakpoint_Event;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class;
       Id       : Integer);
   --  Hook for "debugger_breakpoints_added",
   --  "debugger_breakpoints_changed", debugger_breakpoints_deleted

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

      Location_Box         : Dialog_Group_Widget;
      Subprogram_Box       : Dialog_Group_Widget;
      Address_Box          : Dialog_Group_Widget;
      Exception_Box        : Dialog_Group_Widget;
      Conditions_Box       : Dialog_Group_Widget;
      Commands_Box         : Dialog_Group_Widget;

      Stop_Always_Exception      : Gtk_Radio_Button;
      Stop_Not_Handled_Exception : Gtk_Radio_Button;

      Breakpoint_Type      : Gtk_Combo_Box_Text;

      Exception_Name       : Gtk_Combo_Box_Text;

      File_Name            : Gtk_Entry;
      Line_Spin            : Gtk_Spin_Button;
      Address_Combo        : Gtk_Combo_Box_Text;
      Subprogram_Combo     : Gtk_Combo_Box_Text;

      Temporary            : Gtk_Check_Button;
      Condition_Combo      : Gtk_Combo_Box_Text;
      Ignore_Count_Combo   : Gtk_Spin_Button;
      Command_Descr        : Gtkada_Multiline_Entry;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure Initialize
     (Self   : not null access Properties_Editor_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Create the breakpoint properties editor

   procedure Load_Exceptions
     (Self : not null access Properties_Editor_Record'Class);
   --  Load the debugger-specific exception filters and the predefined ones.

   procedure Fill
     (Self : not null access Properties_Editor_Record'Class;
      Br   : Breakpoint_Data);
   --  Show the information for the given breakpoint in the editor

   procedure Apply
     (Self : not null access Properties_Editor_Record'Class;
      Br   : in out Breakpoint_Data);
   --  Apply the settings to the given breakpoint.

   procedure On_Load_Exception_List_Clicked (W : access GObject_Record'Class);
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

   type View_Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access View_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Show the source editor that has the breakpoint

   type Evaluate_Request is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with null record;

   type Evaluate_Request_Access is access all Evaluate_Request;

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   Exception_Name_Pattern : constant VSS.Regular_Expressions.
     Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
       ("^([^:]+):");
   --  Regexp to find exceptions names

   Props : Properties_Editor;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Self : not null access Properties_Editor_Record'Class;
      Br   : in out Breakpoint_Data)
   is
      T           : constant Breakpoint_Kind :=
        Breakpoint_Kind'Val (Get_Active (Self.Breakpoint_Type));
      Temporary   : constant Boolean := Self.Temporary.Get_Active;
      Condition   : VSS.Strings.Virtual_String;
      Commands    : VSS.Strings.Virtual_String;
      Ignore      : Integer := 0;
      Disposition : Breakpoint_Disposition;
   begin
      if Self.Conditions_Box.Get_Visible then
         Condition := VSS.Strings.Conversions.To_Virtual_String
           (Self.Condition_Combo.Get_Active_Text);
      end if;

      if Self.Ignore_Count_Combo.Get_Visible then
         Ignore :=
           Integer'Max (0,
                        Integer (Get_Value_As_Int (Self.Ignore_Count_Combo)));
      end if;

      if Self.Command_Descr.Get_Visible then
         declare
            Start, The_End : Gtk_Text_Iter;
         begin
            Get_Bounds (Get_Buffer (Self.Command_Descr), Start, The_End);
            Commands := VSS.Strings.Conversions.To_Virtual_String
              (String'
                 (Get_Text (Get_Buffer (Self.Command_Descr), Start, The_End)));
         end;
      end if;

      Disposition := (if Self.Temporary.Get_Active then Delete else Keep);

      case T is
         when On_Line =>
            Br := (Kind           => On_Line,
                   Num            => Br.Num,
                   Continue_Until => Br.Continue_Until,
                   Enabled        => Br.Enabled,
                   Disposition    => Disposition,
                   Condition      => Condition,
                   Ignore         => Ignore,
                   Commands       => Commands,
                   Verified       => False,
                   Location       =>
                      DAP.Types.Breakpoints.Breakpoint_Location_Type'
                     (Marker  => Self.Kernel.Get_Buffer_Factory.Create_Marker
                        (File   => Create (+Get_Text (Self.File_Name)),
                         Line   => Editable_Line_Type'Value
                           (Self.Line_Spin.Get_Text),
                         Column => 1),
                      Address => Invalid_Address));

         when On_Subprogram =>
            Br := (Kind           => On_Subprogram,
                   Num            => Br.Num,
                   Continue_Until => Br.Continue_Until,
                   Enabled        => Br.Enabled,
                   Disposition    => Disposition,
                   Condition      => Condition,
                   Ignore         => Ignore,
                   Commands       => Commands,
                   Verified       => False,
                   Subprogram     =>
                      To_Unbounded_String
                     (Self.Subprogram_Combo.Get_Active_Text));

         when On_Exception =>
            declare
               Exception_Name : Unbounded_String;
            begin
               if Self.Exception_Name.Get_Active = 0 then
                  Exception_Name :=
                    To_Unbounded_String (All_Exceptions_Filter);

               elsif Self.Exception_Name.Get_Active = 1 then
                  Exception_Name := To_Unbounded_String ("assert");

               else
                  Exception_Name :=
                    To_Unbounded_String (Self.Exception_Name.Get_Active_Text);
               end if;

               Br := (Kind           => On_Exception,
                      Num            => Br.Num,
                      Continue_Until => Br.Continue_Until,
                      Enabled        => Br.Enabled,
                      Disposition    => Disposition,
                      Condition      => Condition,
                      Ignore         => Ignore,
                      Commands       => Commands,
                      Verified       => False,
                      Exception_Name => Exception_Name,
                      Unhandled      =>
                        Get_Active (Self.Stop_Not_Handled_Exception));
            end;

         when On_Instruction =>
            Br := (Kind           => On_Instruction,
                   Num            => Br.Num,
                   Continue_Until => Br.Continue_Until,
                   Enabled        => Br.Enabled,
                   Disposition    => Disposition,
                   Condition      => Condition,
                   Ignore         => Ignore,
                   Commands       => Commands,
                   Verified       => False,
                   Location       =>
                      Breakpoint_Location_Type'
                     (Address => String_To_Address
                        (Self.Address_Combo.Get_Active_Text),
                      others  => <>));
      end case;
   end Apply;

   -------------------------------------------
   -- Get_Selected_Breakpoints_Or_Set_State --
   -------------------------------------------

   procedure Get_Selected_Breakpoints_Or_Set_State
     (View    : not null access Breakpoint_View_Record'Class;
      Is_Set  : Boolean;
      State   : Boolean;
      Indexes : in out Breakpoint_Index_Lists.List)
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

      --  Iterate over the selected rows
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
                  --  Get the selected breakpoints' indexes.
                  declare
                     Indices : constant Glib.Gint_Array :=
                       Gtk.Tree_Model.Get_Indices (Path);

                     Index   : constant Integer := Integer
                       (Indices (Indices'First)) + 1;
                     --  Gtk+ indexes start from 0, while breakpoint ones start
                     --  from 1, so add +1 to the Gtk+ index.
                  begin
                     Indexes.Append (Index);
                  end;
               end if;
            end if;

            G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
         end loop;
      end if;
      Free_Path_List (Path_List);
   end Get_Selected_Breakpoints_Or_Set_State;

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

      --  Customize the columns' capabilities, in particular by making
      --  the rows clickable and by allowing to set a custom foreground
      --  color.
      declare
         use Column_List;
         use Cell_Renderer_List;

         List     : Cell_Renderer_List.Glist;
         Renderer : Gtk_Cell_Renderer;
         Columns  : Column_List.Glist;
      begin
         --  Make the 'Enabled' column activatable, to allow the user
         --  enabling/disabling breakpoints.
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

         Columns := Self.List.Get_Columns;

         --  Allow customizing the rows' foreground color, depending on whether
         --  breakpoints have been verified by the DAP server or not.
         while Columns /= Column_List.Null_List loop
            List := Columns.Get_Data.Get_Cells;

            while List /= Cell_Renderer_List.Null_List loop
               Renderer := List.Get_Data;

               if Renderer.all in Gtk_Cell_Renderer_Text_Record'Class then
                  Columns.Get_Data.Add_Attribute
                    (Cell_Renderer_List.Get_Data (List),
                     "foreground-rgba",
                     Col_Fg_Color);
               end if;

               List := List.Next;
            end loop;

            Cell_Renderer_List.Free (List);
            Columns := Column_List.Next (Columns);
         end loop;

         Column_List.Free (Columns);
      end;

      Gtk_New (Self.Multipress, Widget => Self.List);
      Self.Multipress.On_Pressed (On_Multipress'Access, Slot => Self);
      Self.Multipress.Watch (Self);

      Gtk_New (Self.Longpress, Widget => Self.List);
      Self.Longpress.On_Pressed (On_Longpress'Access, Slot => Self);
      Self.Longpress.Watch (Self);

      --  Connect to breakpoints' hooks
      Debugger_Breakpoints_Changed_Hook.Add
        (new On_Breakpoints_Changed, Watch => Self);
      Debugger_Breakpoint_Added_Hook.Add
        (new On_Breakpoint_Event (Added), Watch => Self);
      Debugger_Breakpoint_Changed_Hook.Add
        (new On_Breakpoint_Event (Changed), Watch => Self);
      Debugger_Breakpoint_Deleted_Hook.Add
        (new On_Breakpoint_Event (Deleted), Watch => Self);

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
      Adj       : Gtk_Adjustment;
      Button    : Gtk_Button;
      Main_View : Dialog_View;
      Frame     : Dialog_Group_Widget;
      Hbox      : Gtk_Hbox;
      Dummy     : Gtk_Widget;
      Doc_Label : Gtk_Label;
   begin
      Gtk.Dialog.Initialize
        (Self,
         "Breakpoint editor",
         Parent => Kernel.Get_Main_Window,
         Flags  => Destroy_With_Parent);

      GPS.Main_Window.Set_Default_Size_From_History
        (Self, "breakpoints", Kernel, 600, 500);

      Self.Kernel := Kernel;

      Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Main_View);
      Self.Get_Content_Area.Pack_Start (Main_View);

      ------------
      --  Choosing the type of the breakpoint
      ------------

      Gtk_New (Self.Breakpoint_Type);
      for T in Breakpoint_Kind loop
         case T is
            when On_Line =>
               Self.Breakpoint_Type.Append_Text ("break on source location");
            when On_Subprogram =>
               Self.Breakpoint_Type.Append_Text ("break on subprogram");
            when On_Exception =>
               Self.Breakpoint_Type.Append_Text ("break on exception");
            when On_Instruction =>
               Self.Breakpoint_Type.Append_Text ("break at specific address");
         end case;
      end loop;

      Self.Breakpoint_Type.Set_Active (Breakpoint_Kind'Pos (On_Line));
      Self.Breakpoint_Type.On_Changed (On_Type_Changed'Access, Self);

      Frame := new Dialog_Group_Widget_Record;
      Initialize
        (Frame,
         Parent_View         => Main_View,
         Group_Name          => "Breakpoint Type",
         Allow_Multi_Columns => True);
      Frame.Create_Child
        (Widget    => Self.Breakpoint_Type,
         Doc       => "The type of breakpoint",
         Child_Key => "breakpoint-type-combo");

      Gtk_New (Self.Temporary);
      Self.Temporary.Set_Active (False);
      Frame.Create_Child
        (Widget    => Self.Temporary,
         Label     => "Temporary breakpoint",
         Doc       => "Automatically delete the breakpoint the first time "
         & "it's hit.",
         Child_Key => "breakpoint-temporary-checkbox");

      ------------
      --  Break on source location
      ------------

      Self.Location_Box := new Dialog_Group_Widget_Record;
      Initialize
        (Self.Location_Box,
         Parent_View         => Main_View,
         Group_Name          => "Breakpoint Location",
         Allow_Multi_Columns => True);

      Gtk_New (Self.File_Name);
      Self.File_Name.Set_Editable (True);
      Self.File_Name.Set_Activates_Default (True);
      Self.Location_Box.Create_Child
        (Widget    => Self.File_Name,
         Label     => "File",
         Child_Key => "breakpoint-location-file");

      Gtk_New (Self.Line_Spin, Min => 1.0, Max => 1.0e+08, Step => 1.0);
      Self.Line_Spin.Set_Numeric (True);
      Self.Line_Spin.Set_Value (1.0);
      Self.Line_Spin.Set_Activates_Default (True);
      Self.Location_Box.Create_Child
        (Widget    => Self.Line_Spin,
         Label     => "Line",
         Child_Key => "breakpoint-location-line");

      ------------
      --   Break on subprogram
      ------------

      Self.Subprogram_Box := new Dialog_Group_Widget_Record;
      Initialize
        (Self.Subprogram_Box,
         Parent_View         => Main_View,
         Group_Name          => "Subprogram Breakpoint",
         Allow_Multi_Columns => True);

      Gtk_New_With_Entry (Self.Subprogram_Combo);
      Self.Subprogram_Combo.Append_Text ("");

      Self.Subprogram_Box.Create_Child
        (Widget    => Self.Subprogram_Combo,
         Doc       => "Break on the given subpogram.",
         Child_Key => "breakpoint-subprogram-name");

      ------------
      --  Break_On_Address
      ------------
      Self.Address_Box := new Dialog_Group_Widget_Record;
      Initialize
        (Self.Address_Box,
         Parent_View         => Main_View,
         Group_Name          => "Address Breakpoint",
         Allow_Multi_Columns => True);

      Gtk_New_With_Entry (Self.Address_Combo);
      Self.Address_Combo.Append_Text ("");

      Self.Address_Box.Create_Child
        (Widget    => Self.Address_Combo,
         Doc       => "Break on the instruction located at this address.",
         Child_Key => "breakpoint-address");

      -----------
      --  Break on exception
      -----------

      Self.Exception_Box := new Dialog_Group_Widget_Record;
      Initialize
        (Self.Exception_Box,
         Parent_View         => Main_View,
         Group_Name          => "Breakpoint Exception",
         Allow_Multi_Columns => True);

      Gtk_New_With_Entry (Self.Exception_Name);
      Self.Load_Exceptions;

      Gtk_New (Button, "Load List");
      Button.On_Clicked (On_Load_Exception_List_Clicked'Access, Self);

      Self.Exception_Box.Create_Child
        (Widget    => Self.Exception_Name,
         Doc       => "Break when this exception is raised.",
         Button    => Button,
         Child_Key => "breakpoint-exception-name");

      Gtk_New_Hbox (Hbox, Homogeneous => False);

      Gtk_New (Self.Stop_Always_Exception, Label => "Always stop");
      Self.Stop_Always_Exception.Set_Active (True);
      Hbox.Pack_Start (Self.Stop_Always_Exception, False);

      Gtk_New
        (Self.Stop_Not_Handled_Exception,
         Group => Self.Stop_Always_Exception,
         Label => "Stop if not handled");
      Hbox.Pack_Start (Self.Stop_Not_Handled_Exception, False);

      Self.Exception_Box.Create_Child
        (Widget       => Hbox,
         Label        => "Exception break mode",
         Doc          => "Whether the debugger should always stop on "
         & "exceptions or just the undhanled ones.",
         Child_Key    => "breakpoint-exception-action",
         Expand       => False);

      Self.Conditions_Box := new Dialog_Group_Widget_Record;
      Initialize
        (Self.Conditions_Box,
         Parent_View         => Main_View,
         Group_Name          => "Conditions",
         Allow_Multi_Columns => True);

      --------------
      --  Advanced: condition
      --------------

      Gtk_New_With_Entry (Self.Condition_Combo);
      Self.Conditions_Box.Create_Child
        (Widget    => Self.Condition_Combo,
         Label     => "Condition",
         Doc       => "Break only when the specified condition is true.",
         Child_Key => "breakpoint-condition-combo");

      --------------
      --  Advanced: ignore
      --------------

      Gtk_New (Adj, 0.0, 0.0, 100_000.0, 1.0, 10.0);
      Gtk_New (Self.Ignore_Count_Combo, Adj, 1.0, 0);
      Self.Ignore_Count_Combo.Set_Numeric (False);
      Self.Ignore_Count_Combo.Set_Snap_To_Ticks (True);
      Self.Ignore_Count_Combo.Set_Update_Policy (Update_Always);
      Self.Ignore_Count_Combo.Set_Value (0.0);
      Self.Ignore_Count_Combo.Set_Wrap (False);
      Self.Conditions_Box.Create_Child
        (Widget    => Self.Ignore_Count_Combo,
         Label     => "Ignore",
         Doc       => "Enter the number of times to skip before stopping.",
         Child_Key => "breakpoint-ignore-combo");

      --------------
      --  Advanced: commands
      --------------

      Self.Commands_Box := new Dialog_Group_Widget_Record;
      Initialize
        (Self.Commands_Box,
         Parent_View         => Main_View,
         Group_Name          => "Commands",
         Allow_Multi_Columns => False);

      Gtk_New (Doc_Label, "The commands executed when the breakpoint is hit.");
      Apply_Doc_Style (Doc_Label);
      Self.Commands_Box.Append_Child
        (Doc_Label,
         Expand => False,
         Fill   => False);

      Frame := new Dialog_Group_Widget_Record;
      Initialize
        (Frame,
         Parent_View         => Main_View,
         Allow_Multi_Columns => False);
      Gtk_New (Self.Command_Descr);
      Frame.Append_Child
        (Widget    => Self.Command_Descr,
         Child_Key => "breakpoint-commands",
         Expand    => True);

      --------------
      --  Set proper visibility
      --------------

      Self.Show_All;
      Self.Location_Box.Set_No_Show_All (True);
      Self.Subprogram_Box.Set_No_Show_All (True);
      Self.Exception_Box.Set_No_Show_All (True);
      Self.Address_Box.Set_No_Show_All (True);

      --------------
      --  Fill information
      --------------

      --  Reinitialize the contents of the file name entry
      if DAP.Module.Get_Current_Debugger /= null then
         Set_Text
           (Self.File_Name,
            +Relative_Path
              (DAP.Module.Get_Current_Debugger.Get_Stack_Trace.
                   Get_Current_File,
               Get_Current_Dir));
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

   ---------------------
   -- Load_Exceptions --
   ---------------------

   procedure Load_Exceptions
     (Self : not null access Properties_Editor_Record'Class)
   is
      M            : Gtk_List_Store;
      Client       : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Capabilities : DAP.Tools.Optional_Capabilities;
   begin
      M := -Self.Exception_Name.Get_Model;
      M.Clear;
      Add_Unique_Combo_Entry (Self.Exception_Name, "All Ada exceptions");
      Add_Unique_Combo_Entry (Self.Exception_Name, "Ada assertions");

      if Client /= null
        and then Client.Get_Capabilities.Is_Set
      then
         Capabilities := Client.Get_Capabilities;
         for Index in 3 .. Length
           (Capabilities.Value.exceptionBreakpointFilters)
         loop
            Add_Unique_Combo_Entry
              (Self.Exception_Name,
               UTF8 (Capabilities.Value.exceptionBreakpointFilters
                 (Index).label));
         end loop;
         Self.Exception_Name.Set_Active (0);
      end if;
   end Load_Exceptions;

   ---------------------------------
   -- On_Breakpoint_State_Toggled --
   ---------------------------------

   procedure On_Breakpoint_State_Toggled
     (Self : access Glib.Object.GObject_Record'Class;
      Path : Glib.UTF8_String)
   is
      View    : constant Breakpoint_View := Breakpoint_View (Self);
      Model   : constant Gtk_Tree_Store  := -Get_Model (View.List);
      Iter    : constant Gtk_Tree_Iter   := Model.Get_Iter_From_String (Path);
      Indexes : Breakpoint_Index_Lists.List;
   begin
      if Iter /= Null_Iter then
         Indexes.Append
           (Integer (View.Get_Index_For_Iter (Iter)));
         Set_Breakpoints_State
           (View.Kernel,
            Indexes => Indexes,
            State   => Model.Get_Boolean (Iter, Col_Enb));
      end if;
   end On_Breakpoint_State_Toggled;

   -------------------------
   -- On_Location_Changed --
   -------------------------

   overriding procedure On_Location_Changed
     (Self : not null access Breakpoint_View_Record)
   is
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
   begin
      if Self.Prevent_Bp_Selection
        or else Client = null
        or else Client.Get_Stack_Trace.Get_Current_File = No_File
      then
         return;
      end if;

      --  If the new location has a breakpoint then select it
      declare
         Breakpoints : constant Breakpoint_Vectors.Vector :=
           Client.Get_Breakpoints_Manager.Get_Breakpoints;
      begin
         for Index in Breakpoints.First_Index .. Breakpoints.Last_Index loop
            declare
               Data : constant Breakpoint_Data := Breakpoints (Index);
            begin
               if Data.Kind = On_Line
                 and then Get_File (Data.Location.Marker) =
                 Client.Get_Stack_Trace.Get_Current_File
                 and then Natural (Get_Line (Data.Location.Marker)) =
                   Client.Get_Stack_Trace.Get_Current_Line
               then
                  Self.List.Get_Selection.Unselect_All;
                  Self.List.Get_Selection.Select_Iter
                    (Get_Iter_For_Index (Self, Index));
                  exit;
               end if;
            end;
         end loop;
      end;
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
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      Capabilities : DAP.Tools.Optional_Capabilities :=
        Client.Get_Capabilities;
      Match : VSS.Regular_Expressions.Regular_Expression_Match;
      Old   : ExceptionBreakpointsFilter_Vector;
   begin
      New_Request := null;

      if Capabilities.Is_Set then
         Old := Capabilities.Value.exceptionBreakpointFilters;
         Capabilities.Value.exceptionBreakpointFilters.Clear;
         if Length (Old) >= 1 then
            Capabilities.Value.exceptionBreakpointFilters.Append (Old (1));
         end if;
         if Length (Old) >= 2 then
            Capabilities.Value.exceptionBreakpointFilters.Append (Old (2));
         end if;

         declare
            use VSS.String_Vectors;
            Lines : constant Virtual_String_Vector :=
              Result.a_body.result.Split_Lines;
         begin
            for Index in 2 .. Length (Lines) loop
               declare
                  Line : constant VSS.Strings.Virtual_String :=
                    Element (Lines, Index);
               begin
                  Match := Exception_Name_Pattern.Match (Line);

                  if Match.Has_Match then
                     Capabilities.Value.exceptionBreakpointFilters.Append
                       ((filter            => Match.Captured (1),
                         label             => Match.Captured (1),
                         default           => False,
                         supportsCondition => True,
                         others            => <>));
                  end if;
               end;
            end loop;
         end;
      end if;

      Client.Set_Capabilities (Capabilities);

      if Props /= null then
         Props.Load_Exceptions;
      end if;
   end On_Result_Message;

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
      if Status = Debug_Available
        or else Status = Debug_None
      then
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

   ------------------------
   -- Get_Iter_For_Index --
   ------------------------

   function Get_Iter_For_Index
     (Self  : not null access Breakpoint_View_Record'Class;
      Index : Positive) return Gtk_Tree_Iter
   is
      Model        : constant Gtk_Tree_Store := -Get_Model (Self.List);
      Iter_Indices : constant Gint_Array := (0 => Gint (Index - 1));
      --  Indexes start at 1 and Indices at 0
   begin
      return Model.Get_Iter
        (Gtk_Tree_Path_New_From_Indicesv (Iter_Indices, 1));
   end Get_Iter_For_Index;

   ---------------------------------
   -- Get_Index_For_Breakpoint_Id --
   ---------------------------------

   function Get_Index_For_Breakpoint_Id
     (Self  : not null access Breakpoint_View_Record'Class;
      Num    : Breakpoint_Identifier) return Integer
   is
      Model : constant Gtk_Tree_Store := -Get_Model (Self.List);

      -------------
      -- Convert --
      -------------

      function Convert (Value : String) return Breakpoint_Identifier
      is
        (if Value /= "" then Breakpoint_Identifier'Value (Value)
         else No_Breakpoint);

      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Idx   : Positive := 1;
   begin
      if Num = No_Breakpoint then
         return -1;
      end if;

      Iter := Model.Get_Iter_First;
      while Iter /= Null_Iter loop
         if Convert (Model.Get_String (Iter, Col_Num)) = Num then
            return Idx;
         end if;

         Model.Next (Iter);
         Idx := Idx + 1;
      end loop;

      return -1;
   end Get_Index_For_Breakpoint_Id;

   -----------------------------
   -- Get_Breakpoint_For_Iter --
   -----------------------------

   function Get_Breakpoint_For_Iter
     (Self : not null access Breakpoint_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Breakpoint_Data
   is
      Model        : constant Gtk_Tree_Store := -Get_Model (Self.List);
      Iter_Indices : constant Gint_Array :=
        Get_Indices (Get_Path (Model, Iter));
   begin
      --  Indexes start at 1 and Indices at 0
      return DAP.Module.Breakpoints.Get_Breakpoint_From_Index
        (Integer (Iter_Indices (Iter_Indices'First)) + 1);
   end Get_Breakpoint_For_Iter;

   ---------------------
   -- Get_Id_For_Iter --
   ---------------------

   function Get_Index_For_Iter
     (Self : not null access Breakpoint_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Positive
   is
      Model        : constant Gtk_Tree_Store := -Get_Model (Self.List);
      Iter_Indices : constant Glib.Gint_Array :=
        Get_Indices (Get_Path (Model, Iter));
   begin
      --  Indexes start at 1 and Indices at 0
      return Positive (Iter_Indices (Iter_Indices'First) + 1);
   end Get_Index_For_Iter;

   -----------------------
   -- Update_Breakpoint --
   -----------------------

   procedure Update_Breakpoint
     (Self             : not null access Breakpoint_View_Record'Class;
      Data             : Breakpoint_Data;
      Create_If_Needed : Boolean := True)
   is
      Model           : constant Gtk_Tree_Store := -Get_Model (Self.List);
      Idx             : constant Integer :=
        Self.Get_Index_For_Breakpoint_Id (Data.Num);
      Iter            : Gtk_Tree_Iter :=
        (if Idx = -1 then Null_Iter else Self.Get_Iter_For_Index (Idx));
      Values          : Glib.Values.GValue_Array (1 .. 12);
      Columns         : Columns_Array (Values'Range);
      Last_Column_Idx : Gint;
      Fg_Color        : Gdk_RGBA;
   begin
      --  We did not find any row with the given breakpoint's ID: create a new
      --  row for it if asked, or return immediately.
      if Iter = Null_Iter then
         if Create_If_Needed then
            Append (Model, Iter, Null_Iter);
         else
            return;
         end if;
      end if;

      Columns (1 .. 5) :=
        (Col_Enb, Col_Activatable, Col_Type, Col_Disp, Col_Num);
      Values  (1 .. 5) :=
        (1 => As_Boolean (Data.Enabled),
         2 => As_Boolean (Self.Activatable),
         3 => As_String ("break"),
         4 => As_String (To_Lower (Data.Disposition'Img)),
         5 => As_String (if Data.Num = No_Breakpoint then ""
           else Breakpoint_Identifier'Image (Data.Num)));
      Last_Column_Idx := 5;

      if Data.Kind = On_Line
        and then Data.Location.Marker /= No_Marker
      then
         Last_Column_Idx := Last_Column_Idx + 1;
         Columns (Last_Column_Idx) := Col_File;
         Glib.Values.Init
           (Values (Last_Column_Idx), Column_Types (Guint (Col_File)));

         Glib.Values.Set_String
           (Values (Last_Column_Idx), Escape_Text
            (+Base_Name (Get_File (Get_Location (Data)))));

         Last_Column_Idx := Last_Column_Idx + 1;
         Columns (Last_Column_Idx) := Col_Line;
         Glib.Values.Init_Set_String
           (Values (Last_Column_Idx), Get_Line (Get_Location (Data))'Img);

         if Data.Location.Address /= Invalid_Address then
            Last_Column_Idx := Last_Column_Idx + 1;
            Columns (Last_Column_Idx) := Col_Address;
            Glib.Values.Init_Set_String
              (Values (Last_Column_Idx),
               Escape_Text
                 (Address_To_String
                      (Data.Location.Address)));
         end if;
      end if;

      if Data.Kind = On_Exception then
         Last_Column_Idx := Last_Column_Idx + 1;
         Columns (Last_Column_Idx) := Col_Exception;
         Glib.Values.Init_Set_String
           (Values (Last_Column_Idx),
            Escape_Text (To_String (Data.Exception_Name)));
      end if;

      if Data.Kind = On_Subprogram then
         Last_Column_Idx := Last_Column_Idx + 1;
         Columns (Last_Column_Idx) := Col_Subprogs;
         Glib.Values.Init_Set_String
           (Values (Last_Column_Idx),
            Escape_Text (To_String (Data.Subprogram)));
      end if;

      if Data.Kind = On_Instruction then
         Last_Column_Idx := Last_Column_Idx + 1;
         Columns (Last_Column_Idx) := Col_Address;
         Glib.Values.Init_Set_String
           (Values (Last_Column_Idx),
            Escape_Text (Address_To_String (Data.Location.Address)));
      end if;

      Set_And_Clear
        (Model   => Model,
         Iter    => Iter,
         Columns => Columns (1 .. Last_Column_Idx),
         Values  => Values (1 .. Last_Column_Idx));

      --  Gray out breakpoint's row if the breakpoint is not verified by the
      --  server (e.g: pending breakpoints).
      Fg_Color :=
        (if Data.Verified then
            GPS.Kernel.Preferences.Default_Style.Get_Pref_Fg
         else
            Background
           (GPS.Default_Styles.Editor_Code_Annotations_Style));

      Model.Set_Value (Iter, Col_Fg_Color, As_RGBA (Fg_Color));
   end Update_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   procedure Remove_Breakpoint
     (Self : not null access Breakpoint_View_Record'Class;
      Id   : Breakpoint_Identifier)
   is
      Model          : constant Gtk_Tree_Store := -Get_Model (Self.List);
      Idx            : constant Integer :=
        Self.Get_Index_For_Breakpoint_Id (Id);
      Iter_To_Remove : Gtk_Tree_Iter :=
        (if Idx = -1 then Null_Iter else Self.Get_Iter_For_Index (Idx));
   begin
      if Iter_To_Remove /= Null_Iter then
         Model.Remove (Iter_To_Remove);
      end if;
   end Remove_Breakpoint;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Breakpoint_View_Record)
   is
      Client  : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Model   : constant Gtk_Tree_Store := -Get_Model (View.List);
   begin
      Clear (Model);

      if Client = null then
         Trace
           (Me, "Add"
            & DAP.Module.Breakpoints.Get_Persistent_Breakpoints.Length'Img
            & " persistent breakpoints");

         for Data of DAP.Module.Breakpoints.Get_Persistent_Breakpoints loop
            View.Update_Breakpoint (Data);
         end loop;
      else
         Trace
           (Me, "Add"
            & Client.Get_Breakpoints_Manager.Get_Breakpoints.Length'Img
            & " debugger breakpoints");

         for Data of Client.Get_Breakpoints_Manager.Get_Breakpoints loop
            View.Update_Breakpoint (Data);
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

   overriding procedure Execute
      (Self     : On_Breakpoint_Event;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class;
       Id       : Integer)
   is
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
         case Self.Event is
            when Added =>
               View.Update_Breakpoint
                 (Data => DAP.Module.Breakpoints.Get_Breakpoint_From_Id
                    (Breakpoint_Identifier (Id)),
                  Create_If_Needed => True);
            when Changed =>
               View.Update_Breakpoint
                 (Data => DAP.Module.Breakpoints.Get_Breakpoint_From_Id
                    (Breakpoint_Identifier (Id)),
                  Create_If_Needed => False);
            when Deleted =>
               View.Remove_Breakpoint
                 (Breakpoint_Identifier (Id));
         end case;
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
     (Command : access View_Breakpoint_Command;
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
         Show_Selected_Breakpoint_In_Editor (View);
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
      Indexes : Breakpoint_Index_Lists.List;
   begin
      --  Get the list of selected breakpoints
      Get_Selected_Breakpoints_Or_Set_State
        (View    => View,
         Is_Set  => False,
         State   => False,
         Indexes => Indexes);

      --  Delete them
      Delete_Multiple_Breakpoints (Kernel, Indexes);
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
      Indexes : Breakpoint_Index_Lists.List;
   begin
      if View = null then
         return Commands.Failure;
      end if;

      --  Get the list of selected breakpoints
      Get_Selected_Breakpoints_Or_Set_State
        (View    => View,
         Is_Set  => False,
         State   => Command.Is_Enabled,
         Indexes => Indexes);

      --  Enable/disable the selected breakpoints
      Set_Breakpoints_State (View.Kernel, Indexes, Command.Is_Enabled);

      --  Need to modify the toggle buttons in the model
      Get_Selected_Breakpoints_Or_Set_State
        (View    => View,
         Is_Set  => True,
         State   => Command.Is_Enabled,
         Indexes => Indexes);

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
      View : constant Breakpoint_View :=
        Breakpoint_View
          (Breakpoints_MDI_Views.Retrieve_View
             (Get_Kernel (Context.Context),
              Visible_Only => True));
      Br   : Breakpoint_Data := Empty_Breakpoint_Data;
   begin
      if View /= null then
         Props := new Properties_Editor_Record;
         Initialize (Props, View.Kernel);
         Fill (Props, Br);

         --  The user pressed on 'Apply': create the actual breakpoint,
         --  assigning it a new ID.
         if Props.Run = Gtk_Response_Apply then
            Apply (Props, Br);
            Break (View.Kernel, Br);
         end if;

         Props.Destroy;
         Props := null;
      end if;

      return Success;
   end Execute;

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
   begin
      Current := Get_Selection (View);
      if Current /= Empty_Breakpoint_Data then
         Props := new Properties_Editor_Record;
         Initialize (Props, View.Kernel);
         Fill (Props, Current);

         if Props.Run = Gtk_Response_Apply then
            Apply (Props, Current);
            Set_Breakpoint_At_Index
              (View.Kernel, Current, Get_Selection_Index (View));
         end if;

         Props.Destroy;
         Props := null;
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

      if Selection /= Empty_Breakpoint_Data
        and then Selection.Kind = On_Line
      then
         View.Prevent_Bp_Selection := True;
         DAP.Utils.Goto_Location
           (Kernel    => View.Kernel,
            File      => Get_File (Selection.Location.Marker),
            Line      => Natural
              (Get_Line (Selection.Location.Marker)));
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
            return View.Get_Breakpoint_For_Iter (Iter);
         end if;
      end if;

      return Empty_Breakpoint_Data;
   end Get_Selection;

   -------------------------
   -- Get_Selection_Index --
   -------------------------

   function Get_Selection_Index
     (View : access Breakpoint_View_Record'Class) return Integer
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
            return Integer (View.Get_Index_For_Iter (Iter));
         end if;
      end if;

      return 0;
   end Get_Selection_Index;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (W : access GObject_Record'Class)
   is
      View   : constant Properties_Editor := Properties_Editor (W);
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;

   begin
      if Client = null then
         Add_Unique_Combo_Entry
           (View.Exception_Name, "All Ada exceptions");
         Add_Unique_Combo_Entry
           (View.Exception_Name, "Ada assertions");

      else
         declare
            Req : Evaluate_Request_Access :=
              new Evaluate_Request (View.Kernel);
         begin
            Req.Parameters.arguments.expression := "info exceptions";
            Req.Parameters.arguments.frameId :=
              Client.Get_Stack_Trace.Get_Current_Frame_Id;
            Req.Parameters.arguments.context :=
              (Is_Set => True, Value => DAP.Tools.Enum.repl);
            Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;
      end if;
   end On_Load_Exception_List_Clicked;

   ---------------------
   -- On_Type_Changed --
   ---------------------

   procedure On_Type_Changed (W : access GObject_Record'Class) is
      Self : constant Properties_Editor := Properties_Editor (W);
      T    : constant Breakpoint_Kind :=
        Breakpoint_Kind'Val (Get_Active (Self.Breakpoint_Type));
   begin
      Self.Location_Box.Set_Sensitive (T = On_Line);
      Self.Location_Box.Set_Visible (T = On_Line);

      Self.Subprogram_Box.Set_Sensitive (T = On_Subprogram);
      Self.Subprogram_Box.Set_Visible (T = On_Subprogram);

      Self.Exception_Box.Set_Sensitive (T = On_Exception);
      Self.Exception_Box.Set_Visible (T = On_Exception);

      Self.Address_Box.Set_Sensitive (T = On_Instruction);
      Self.Address_Box.Set_Visible (T = On_Instruction);

      Self.Conditions_Box.Set_Visible (T /= On_Exception);
      Self.Commands_Box.Set_Visible (T /= On_Exception);
      Self.Command_Descr.Set_Visible (T /= On_Exception);
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
      if Br.Kind = On_Exception then
         Self.Breakpoint_Type.Set_Active (Breakpoint_Kind'Pos (On_Exception));
         Set_Active (Self.Stop_Always_Exception, True);

         if Br.Exception_Name = "all" then
            Set_Active_Text (Self.Exception_Name, "All Ada exceptions");
         elsif Br.Exception_Name = "unhandled" then
            Set_Active_Text (Self.Exception_Name, "All Ada exceptions");
            Set_Active (Self.Stop_Not_Handled_Exception, True);
         else
            Add_Unique_Combo_Entry
              (Self.Exception_Name,
               To_String (Br.Exception_Name), Select_Text => True);
         end if;

         Set_Active (Self.Temporary, Br.Disposition /= Keep);

      elsif Br.Kind = On_Subprogram then
         Self.Breakpoint_Type.Set_Active (Breakpoint_Kind'Pos (On_Subprogram));

         Add_Unique_Combo_Entry
           (Self.Subprogram_Combo, To_String (Br.Subprogram), True);

      elsif Br.Kind = On_Instruction then
         Self.Breakpoint_Type.Set_Active
           (Breakpoint_Kind'Pos (On_Instruction));

         Add_Unique_Combo_Entry
           (Self.Address_Combo, Address_To_String (Br.Location.Address));
         Set_Text
           (Gtk_Entry (Self.Address_Combo.Get_Child),
            Address_To_String (Br.Location.Address));

      elsif Br.Kind = On_Line
        and then Br.Location.Marker /= No_Marker
      then
         Set_Text
           (Self.File_Name,
            +Full_Name (Get_File (Br.Location.Marker)));
         Set_Value
           (Self.Line_Spin,
            Grange_Float (Get_Line (Br.Location.Marker)));
      end if;

      --  When editing an existing breakpoint, we can't change its type
      if Br.Num /= No_Breakpoint then
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

      if not Br.Condition.Is_Empty then
         Add_Unique_Combo_Entry
           (Self.Condition_Combo,
            UTF8 (Br.Condition),
            Select_Text => True);
      else
         Self.Condition_Combo.Set_Active (-1);
      end if;

      --  Advanced: ignore

      Self.Ignore_Count_Combo.Set_Value (Grange_Float (Br.Ignore));

      --  Advanced: commands

      Buffer := Get_Buffer (Self.Command_Descr);
      Get_Bounds (Buffer, Start, The_End);
      Delete (Buffer, Start, The_End);

      if not Br.Commands.Is_Empty then
         Insert_At_Cursor (Buffer, UTF8 (Br.Commands));
      end if;
   end Fill;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Selection_Filter   : constant Action_Filter :=
        new Breakpoint_Single_Selection;

      No_Or_Ready_Filter : Action_Filter;
      Dummy              : Action_Filter;
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open breakpoints editor",
         Description => "Open the Breakpoints Editor for the debugger");

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

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug view breakpoint", new View_Breakpoint_Command,
         "View the source editor containing the selected breakpoint"
           & " (from the Breakpoints view)",
         Icon_Name => "gps-goto-symbolic",
         Category  => "Debug",
         Filter    => Selection_Filter);
   end Register_Module;

end DAP.Views.Breakpoints;
