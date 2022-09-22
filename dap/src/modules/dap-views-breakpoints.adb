------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Toggle;   use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;

with GPS.Debuggers;              use GPS.Debuggers;
with GPS.Editors;                use GPS.Editors;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Markers;                use GPS.Markers;
with GPS.Kernel.Actions;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;

with DAP.Types;                  use DAP.Types;
with DAP.Breakpoint_Maps;        use DAP.Breakpoint_Maps;
with DAP.Persistent_Breakpoints; use DAP.Persistent_Breakpoints;
with DAP.Clients;                use DAP.Clients;
with DAP.Module;

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

   Column_Types : constant Glib.GType_Array (0 .. 9) :=
     (Guint (Col_Enb)         => GType_Boolean,
      Guint (Col_Activatable) => GType_Boolean,
      others                  => GType_String);

   Column_Names : constant GNAT.Strings.String_List (1 .. 10) :=
     (new String'("Num"),
      new String'("Enb"),
      new String'("Type"),
      new String'("Disp"),
      new String'("File/Variable"),
      new String'("Line"),
      new String'("Exception"),
      new String'("Subprograms"),
      new String'("Address"),
      new String'("Activatable"));

   type Breakpoint_View_Record is new View_Record with
      record
         List                 : Gtk_Tree_View;
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

   type No_View_Filter is new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access No_View_Filter;
      Context : Selection_Context) return Boolean;

   type Dummy_Filter is new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Dummy_Filter;
      Context : Selection_Context) return Boolean;

   type On_Breakpoints_Changed is new Debugger_Hooks_Function
      with null record;
   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class);
   --  Hook for "debugger_breakpoints_changed"

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

   type Dummy_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Dummy_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
     is (Success);

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
      return Breakpoint_View (Client.Get_Breakpoints_View);
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

      Debugger_Breakpoints_Changed_Hook.Add
         (new On_Breakpoints_Changed, Watch => Self);

      --  Initial display
      Update (Self);

      return Gtk_Widget (Self.List);
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
      Client : DAP.Clients.DAP_Client_Access := Get_Client (Self);
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
         for Vector of Client.Get_Breakpoints.Sources loop
            for Data of Vector loop
               if Data.Location /= No_Marker
                 and then Get_File (Data.Location) = Client.Current_File
                 and then Natural
                   (Get_Line (Data.Location)) = Client.Current_Line
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
         Clear (-Get_Model (View.List));
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
      if Client.Get_Breakpoints_View /= null then
         Breakpoint_View (Client.Get_Breakpoints_View).On_Process_Terminated;
      end if;

      Client.Set_Breakpoints_View (Generic_Views.Abstract_View_Access (View));
   end Set_View;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Breakpoint_View_Record)
   is
      Client  : DAP.Clients.DAP_Client_Access := Get_Client (View);
      Model   : constant Gtk_Tree_Store := -Get_Model (View.List);
      Iter    : Gtk_Tree_Iter;
      Values  : Glib.Values.GValue_Array (1 .. 10);
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
            2 => As_Boolean (Data.Enabled),
            3 => As_Boolean (View.Activatable));
         Last := 5;

         --  case Data.The_Type is
         --     when Breakpoint =>
         Glib.Values.Init_Set_String (Values (4), "break");
            --  when Watchpoint =>
            --     Glib.Values.Init_Set_String (Values (4), "watch");
            --  when Catchpoint =>
            --     Glib.Values.Init_Set_String (Values (4), "catch");
            --  when Other =>
            --     Glib.Values.Init_Set_String
            --    (Values (4), Escape_Text (To_String (Data.The_Type_Name)));
         --  end case;
         Glib.Values.Init_Set_String
           (Values (5), To_Lower (Data.Disposition'Img));

         --  if Br.Expression /= "" then
         --     Last := Last + 1;
         --     Columns (Last) := Col_File;
         --     Glib.Values.Init_Set_String
         --       (Values (Last), To_String (Br.Expression));
         --  end if;

         if Data.Location /= No_Marker then
            if Last < 6 then
               Last := Last + 1;
               Columns (Last) := Col_File;
               Glib.Values.Init
                 (Values (Last), Column_Types (Guint (Col_File)));
            end if;
            Glib.Values.Set_String
              (Values (Last), Escape_Text
               (+Base_Name (Get_File (Data.Location))));

            Last := Last + 1;
            Columns (Last) := Col_Line;
            Glib.Values.Init_Set_String
              (Values (Last), Get_Line (Data.Location)'Img);
         end if;

         --  if Br.Except /= "" then
         --     Last := Last + 1;
         --     Columns (Last) := Col_Exception;
         --     Glib.Values.Init_Set_String
         --       (Values (Last), Escape_Text (To_String (Br.Except)));
         --  end if;

         if Data.Subprogram /= "" then
            Last := Last + 1;
            Columns (Last) := Col_Subprogs;
            Glib.Values.Init_Set_String
              (Values (Last), Escape_Text (To_String (Data.Subprogram)));
         end if;

         if Data.Address /= Invalid_Address then
            Last := Last + 1;
            Columns (Last) := Col_Address;
            Glib.Values.Init_Set_String
          (Values (Last), Escape_Text (Address_To_String (Data.Address)));
         end if;

         Set_And_Clear (Model, Iter, Columns (1 .. Last), Values (1 .. Last));
      end Fill;

   begin
      Trace (Me, "Update" & DAP.Persistent_Breakpoints.
           Get_Persistent_Breakpoints.Sources.Length'Img);
      --  If the view is being detached (but the process has not been reset
      --  yet), we load the list of persistent breakpoints
      if Client /= null and then Get_View (Client) = null then
         Client := null;
      end if;

      Clear (Model);

      if Client = null then
         for Vector of DAP.Persistent_Breakpoints.
           Get_Persistent_Breakpoints.Sources
         loop
            for Data of Vector loop
               Fill (Data);
            end loop;
         end loop;

         for Data of DAP.Persistent_Breakpoints.Get_Persistent_Breakpoints.
           Subprograms
         loop
            Fill (Data);
         end loop;

      else
         for Vector of Client.Get_Breakpoints.Sources loop
            for Data of Vector loop
               Fill (Data);
            end loop;
         end loop;

         for Data of Client.Get_Breakpoints.Subprograms loop
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
      pragma Unreferenced (Self, Kernel);
      View : Breakpoint_View;

   begin
      if Debugger /= null then
         View := Get_View
           (DAP.Clients.Visual_Debugger_Access (Debugger).Client);
      end if;

      if View /= null then
         Update (View);
      end if;
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
         Clear (-Get_Model (View.List));
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
     (Filter  : access Dummy_Filter;
      Context : Selection_Context) return Boolean is
   begin
      return False;
   end Filter_Matches_Primitive;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      F                  : Action_Filter := new No_View_Filter;
      No_Debugger_Filter : Action_Filter;
      Dummy              : Action_Filter;
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open breakpoints editor",
         Description => "Open the Breakpoints Editor for the debugger",
         Filter      => F);

      No_Debugger_Filter := Kernel.Lookup_Filter ("No debugger or stopped");

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug delete breakpoint", new Remove_Breakpoint_Command,
         "Delete the selected breakpoints"
           & " (from the Breakpoints view)",
         Icon_Name => "gps-remove-symbolic",
         Category  => "Debug",
         Filter    => No_Debugger_Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug clear breakpoints", new Clear_Breakpoints_Command,
         "Delete all existing breakpoints",
         Icon_Name => "gps-clear-symbolic",
         Category  => "Debug",
         Filter    => No_Debugger_Filter);

      ---
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

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug edit breakpoint", new Dummy_Command,
         "Edit the advanced properties of the selected breakpoint"
           & " like its condition, repeat count,..."
           & " (from the Breakpoints view)",
         Icon_Name => "gps-settings-symbolic",
         Category  => "Debug",
         Filter    => Dummy);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug create breakpoint", new Dummy_Command,
         "Create a new breakpoint, from the Breakpoints view",
         Icon_Name => "gps-add-symbolic",
         Category  => "Debug",
         Filter    => Dummy);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug enable breakpoints",
         new Dummy_Command,
         "Enable the selected breakpoints",
         Icon_Name => "gps-syntax-check-symbolic",
         Category  => "Debug",
         Filter    => Dummy);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug disable breakpoints",
         new Dummy_Command,
         "Disable the selected breakpoints",
         Icon_Name => "gps-stop-symbolic",
         Category  => "Debug",
         Filter    => Dummy);
   end Register_Module;

end DAP.Views.Breakpoints;
