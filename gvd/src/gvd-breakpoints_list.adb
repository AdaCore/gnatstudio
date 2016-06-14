------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with Commands;                     use Commands;
with Commands.Interactive;         use Commands.Interactive;
with Debugger;                     use Debugger;
with Debugger_Pixmaps;             use Debugger_Pixmaps;
with GNATCOLL.Scripts;             use GNATCOLL.Scripts;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;       use GPS.Kernel.Preferences;
with GPS.Kernel.Project;           use GPS.Kernel.Project;
with GPS.Kernel.Properties;        use GPS.Kernel.Properties;
with GPS.Intl;                     use GPS.Intl;
with GPS.Properties;               use GPS.Properties;
with GVD_Module;                   use GVD_Module;
with GVD.Preferences;              use GVD.Preferences;
with GVD.Process;                  use GVD.Process;
with XML_Utils;                    use XML_Utils;
with Xref;                         use Xref;

package body GVD.Breakpoints_List is

   Me : constant Trace_Handle := Create ("Breakpoints");

   type Breakpoints_Module is new Module_ID_Record with record
      Breakpoints : Breakpoint_List;
      --  The list of persistent breakpoints for the current project. This
      --  list can be manipulated even when no debugger is running, and is
      --  loaded/saved to disk as needed

      Editor_Breakpoints : Breakpoint_List;
      --  The list of breakpoints currently highlighted in editors. This
   end record;

   Module : access Breakpoints_Module;

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project changes. This is a good time to load the
   --  persistent breakpoints

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called before the project is changed. This is a good time to save the
   --  persistent breakpoints

   type On_Before_Exit is new Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean;
   --  Called before GPS exist. This is a good time to save the persistent
   --  breakpoints.

   type On_Debugger_Terminated is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Terminated;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when one debugger terminates. This is a good time to save
   --  persistent breakpoints.

   type On_Debugger_Started is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Started;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when one debugger starts. The persistent breakpoints are applied.

   type On_Debugger_Location_Changed is
     new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Debugger_Location_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Called when the current location has changed in the debugger.
   --  This is a good time to show, on the side of the editor, which lines
   --  have breakpoints.

   type On_Lines_Revealed is new Context_Hooks_Function with null record;
   overriding procedure Execute
     (Self    : On_Lines_Revealed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context);
   --  Called when a range of lines becomes visible on screen.
   --  It shows lines with ranges.

   procedure Show_Breakpoints_In_All_Editors
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Update the side column for all editors, and show the breakpoints info

   procedure Add_Information
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Info    : in out Line_Information_Array;
      B       : Breakpoint_Data;
      Removed : Boolean);
   --  Prepare the info to display on the side column for that breakpoint.
   --  Removed should be True if the breakpoint is actually being removed.

   procedure Create_Breakpoints_Column
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   procedure Remove_Breakpoints_Column
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Create a new column on the size of the editors for File, where we can
   --  display breakpoint information.

   --------------
   -- Commands --
   --------------

   Breakpoints_Column_Id : constant String := "Debugger/Breakpoints";

   type Breakpoint_Command_Mode is (Set, Unset);
   type Set_Breakpoint_Command_At_Line is new Root_Command with record
      File     : GNATCOLL.VFS.Virtual_File;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Line     : Positive;
      Mode     : Breakpoint_Command_Mode;
   end record;
   overriding function Execute
     (Self : access Set_Breakpoint_Command_At_Line) return Command_Return_Type;

   function Create_Set_Breakpoint_Command
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Positive) return Command_Access;
   --  Create a new instance of the command that sets or removes a breakpoint
   --  at a specific location.

   type Set_Breakpoint_Command_Context is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   overriding function Execute
     (Command : access Set_Breakpoint_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Set a breakpoint at the line given in the context

   -------------
   -- Filters --
   -------------

   type Subprogram_Variable_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type No_Debugger_Or_Stopped_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Stopped_Filter;
      Context : Selection_Context) return Boolean;

   ----------------
   -- Properties --
   ----------------

   type Breakpoint_Property_Record is new Property_Record with record
      Breakpoints : Breakpoint_Vectors.Vector;
   end record;
   type Breakpoint_Property is access all Breakpoint_Property_Record'Class;
   overriding procedure Save
     (Property : access Breakpoint_Property_Record;
      Node     : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out Breakpoint_Property_Record;
      From     : XML_Utils.Node_Ptr);

   procedure Save_Persistent_Breakpoints
     (Kernel   : not null access Kernel_Handle_Record'Class);
   --  Save persistent breakpoints to properties.

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access No_Debugger_Or_Stopped_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context)));
   begin
      return Process = null or else not Command_In_Process (Process);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Fuzzy (Entity) or else Is_Subprogram (Entity);
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

   -----------------------------------
   -- Create_Set_Breakpoint_Command --
   -----------------------------------

   function Create_Set_Breakpoint_Command
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : Breakpoint_Command_Mode;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Positive) return Command_Access
   is
   begin
      return new Set_Breakpoint_Command_At_Line'
        (Root_Command with
         Kernel => Kernel, Mode => Mode, File => File, Line => Line);
   end Create_Set_Breakpoint_Command;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Natural;
      Temporary     : Boolean := False)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class);
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class)
      is
         Num      : Breakpoint_Identifier with Unreferenced;
      begin
         if Self.Command_In_Process then
            Insert
              (Kernel,
               -"The debugger is busy processing a command",
               Mode => Error);
         else
            Num := Visual_Debugger (Self).Debugger.Break_Source
              (File, Line, Temporary => Temporary);
         end if;
      end On_Debugger;

   begin
      if Process = null then
         Module.Breakpoints.List.Append
           (Breakpoint_Data'
              (File   => File,
               Line   => Line,
               Disposition => (if Temporary then Delete else Keep),
               others => <>));
         Show_Breakpoints_In_All_Editors (Kernel);
      else
         For_Each_Debugger (Kernel, On_Debugger'Access);
      end if;
   end Break_Source;

   --------------------
   -- Unbreak_Source --
   --------------------

   procedure Unbreak_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Natural)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class);
      --  Remove a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class) is
      begin
         if Self.Command_In_Process then
            Insert
              (Kernel,
               -"The debugger is busy processing a command",
               Mode => Error);
         else
            Visual_Debugger (Self).Debugger.Remove_Breakpoint_At
              (File, Line, Mode => Visible);
         end if;
      end On_Debugger;

   begin
      if Process = null then
         for Idx in Module.Breakpoints.List.First_Index
           .. Module.Breakpoints.List.Last_Index
         loop
            if Module.Breakpoints.List (Idx).File = File
              and then Module.Breakpoints.List (Idx).Line = Line
            then
               Module.Breakpoints.List.Delete (Idx);
               exit;
            end if;
         end loop;
         Show_Breakpoints_In_All_Editors (Kernel);
      else
         For_Each_Debugger (Kernel, On_Debugger'Access);
      end if;
   end Unbreak_Source;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Subprogram    : String;
      Temporary     : Boolean := False)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      --  Set a breakpoint in a specific instance of the debugger

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class);

      procedure On_Debugger
        (Self : not null access Base_Visual_Debugger'Class)
      is
         Num      : Breakpoint_Identifier with Unreferenced;
      begin
         if Self.Command_In_Process then
            Insert
              (Kernel,
               -"The debugger is busy processing a command",
               Mode => Error);
         else
            Num := Process.Debugger.Break_Subprogram
              (Subprogram, Temporary => Temporary, Mode => GVD.Types.Visible);
         end if;
      end On_Debugger;

   begin
      if Process = null then
         Module.Breakpoints.List.Append
           (Breakpoint_Data'
              (Subprogram => To_Unbounded_String (Subprogram),
               Disposition => (if Temporary then Delete else Keep),
               others     => <>));
         Show_Breakpoints_In_All_Editors (Kernel);
      else
         For_Each_Debugger (Kernel, On_Debugger'Access);
      end if;
   end Break_Subprogram;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Set_Breakpoint_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
      Num      : Breakpoint_Identifier with Unreferenced;

   begin
      if Command.Continue_Till then
         --  Only works if there is a current debugger
         if Process /= null then
            Num := Process.Debugger.Break_Source
              (File_Information (Context.Context),
               GPS.Kernel.Contexts.Line_Information (Context.Context),
               Temporary => True);
            Process.Debugger.Continue (Mode => GVD.Types.Visible);
         end if;

      elsif Command.On_Line then
         Break_Source
           (Kernel,
            File  => File_Information (Context.Context),
            Line  => GPS.Kernel.Contexts.Line_Information (Context.Context));
      else
         Break_Subprogram
           (Kernel,
            Subprogram => Entity_Name_Information (Context.Context));
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Set_Breakpoint_Command_At_Line) return Command_Return_Type
   is
   begin
      case Self.Mode is
         when Set =>
            Break_Source
              (Self.Kernel, File => Self.File, Line => Self.Line);

         when Unset =>
            Unbreak_Source
              (Self.Kernel, File => Self.File, Line => Self.Line);
      end case;
      return Success;
   end Execute;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Breakpoint_Property_Record;
      Node     : in out XML_Utils.Node_Ptr)
   is
      X : Node_Ptr;
   begin
      Trace (Me, "Saving breakpoints for future sessions");
      for B of reverse Property.Breakpoints loop
         X := new XML_Utils.Node;
         X.Tag := new String'("breakpoint");
         X.Next := Node.Child;
         Node.Child := X;
         if B.The_Type /= Breakpoint then
            Set_Attribute (X, "type", Breakpoint_Type'Image (B.The_Type));
         end if;
         if B.Disposition /= Keep then
            Set_Attribute
              (X, "disposition", Breakpoint_Disposition'Image (B.Disposition));
         end if;
         if not B.Enabled then
            Set_Attribute (X, "enabled", "false");
         end if;
         if B.Expression /= "" then
            Set_Attribute (X, "expression", To_String (B.Expression));
         end if;
         if B.File /= GNATCOLL.VFS.No_File then
            Add_File_Child (X, "file", B.File);
         end if;
         if B.Except /= "" then
            Set_Attribute (X, "exception", To_String (B.Except));
         end if;
         if B.Subprogram /= "" then
            Set_Attribute (X, "subprogram", To_String (B.Subprogram));
         end if;
         if B.Line /= 0 then
            Set_Attribute (X, "line", Integer'Image (B.Line));
         end if;
         if B.Address /= Invalid_Address then
            Set_Attribute (X, "address", Address_To_String (B.Address));
         end if;
         if B.Ignore /= 0 then
            Set_Attribute (X, "ignore", Integer'Image (B.Ignore));
         end if;
         if B.Condition /= "" then
            Set_Attribute (X, "condition", To_String (B.Condition));
         end if;
         if B.Commands /= "" then
            Set_Attribute (X, "command", To_String (B.Commands));
         end if;
         if B.Scope /= No_Scope then
            Set_Attribute (X, "scope", Scope_Type'Image (B.Scope));
         end if;
         if B.Action /= No_Action then
            Set_Attribute (X, "action", Action_Type'Image (B.Action));
         end if;
      end loop;
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Breakpoint_Property_Record;
      From     : XML_Utils.Node_Ptr)
   is
      X : Node_Ptr;
      B : Breakpoint_Data;

      function Get_String (Attr : String) return Unbounded_String;
      --  return the value of Attr (or null if the Attr doesn't exist

      ----------------
      -- Get_String --
      ----------------

      function Get_String (Attr : String) return Unbounded_String is
         Value : constant String := Get_Attribute (X, Attr, "");
      begin
         if Value = "" then
            return Null_Unbounded_String;
         else
            return To_Unbounded_String (Value);
         end if;
      end Get_String;

   begin
      Trace (Me, "Restoring breakpoints from previous session");

      X := From.Child;
      while X /= null loop
         B :=
           (Num         => Breakpoint_Identifier'Last,
            Trigger     => Write,
            The_Type => Breakpoint_Type'Value
              (Get_Attribute
                 (X, "type", Breakpoint_Type'Image (Breakpoint))),
            Disposition => Breakpoint_Disposition'Value
              (Get_Attribute
                   (X, "disposition", Breakpoint_Disposition'Image (Keep))),
            Enabled    => Boolean'Value (Get_Attribute (X, "enabled", "true")),
            Expression => Get_String ("expression"),
            Except     => Get_String ("exception"),
            Subprogram => Get_String ("subprogram"),
            Line       => Integer'Value (Get_Attribute (X, "line", "0")),
            Address    => String_To_Address (Get_Attribute (X, "address", "")),
            Ignore     => Integer'Value (Get_Attribute (X, "ignore", "0")),
            Condition  => Get_String ("condition"),
            Commands   => Get_String ("command"),
            Scope      => Scope_Type'Value
              (Get_Attribute (X, "scope", Scope_Type'Image (No_Scope))),
            Action     => Action_Type'Value
              (Get_Attribute (X, "action", Action_Type'Image (No_Action))),
            File        => Get_File_Child (X, "file"));
         Property.Breakpoints.Append (B);
         X := X.Next;
      end loop;
   end Load;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Prop  : Breakpoint_Property_Record;
      Found : Boolean;
   begin
      Module.Breakpoints.List.Clear;

      if not Preserve_State_On_Exit.Get_Pref then
         Trace (Me, "Not loading persistent breakpoints");
         return;
      end if;

      Trace (Me, "Loading persistent breakpoints");
      Get_Property
        (Prop, Get_Project (Kernel), Name => "breakpoints", Found => Found);
      if Found then
         Module.Breakpoints.List := Prop.Breakpoints;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, File);
   begin
      Save_Persistent_Breakpoints (Kernel);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Save_Persistent_Breakpoints (Kernel);
      return True;  --  allow exit
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Terminated;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
   begin
      --  In case the user has set breakpoints manually via the console,
      --  synchronize the global list of breakpoints

      Module.Breakpoints.List.Clear;

      if Break_On_Exception.Get_Pref then
         for B of Visual_Debugger (Debugger).Breakpoints.List loop
            if B.Except = "" or else B.Except /= "all" then
               Module.Breakpoints.List.Append (B);
            end if;
         end loop;
      else
         Module.Breakpoints := Visual_Debugger (Debugger).Breakpoints;
      end if;

      Save_Persistent_Breakpoints (Kernel);
   end Execute;

   ---------------------------------
   -- Save_Persistent_Breakpoints --
   ---------------------------------

   procedure Save_Persistent_Breakpoints
     (Kernel   : not null access Kernel_Handle_Record'Class)
   is
      Prop : Breakpoint_Property;
   begin
      if not Preserve_State_On_Exit.Get_Pref then
         Trace (Me, "Not saving persistent breakpoints");
         return;
      end if;

      if Module.Breakpoints.List.Is_Empty then
         Trace (Me, "No persistent breakpoint to save");
         Remove_Property (Kernel, Get_Project (Kernel), "breakpoints");
         return;
      end if;

      Trace (Me, "Saving persistent breakpoints");
      Prop := new Breakpoint_Property_Record;

      --  Filter breakpoints that are created automatically by GPS as a
      --  result of preferences.

      Prop.Breakpoints := Module.Breakpoints.List;
      Set_Property
        (Kernel, Get_Project (Kernel), "breakpoints", Prop,
         Persistent => True);
   end Save_Persistent_Breakpoints;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Started;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Process : constant Visual_Debugger := Visual_Debugger (Debugger);
      Id      : Breakpoint_Identifier;
   begin
      if not Preserve_State_On_Exit.Get_Pref then
         return;
      end if;

      for B of Module.Breakpoints.List loop
         if B.Except /= "" then
            Id := Process.Debugger.Break_Exception
              (To_String (B.Except),
               Temporary => B.Disposition /= Keep, Mode => Internal,
               Unhandled => False);
         elsif B.Line /= 0 and then B.File /= GNATCOLL.VFS.No_File then
            Id := Process.Debugger.Break_Source
              (B.File, B.Line,
               Temporary => B.Disposition /= Keep, Mode => Internal);
         elsif B.Subprogram /= "" then
            Id := Process.Debugger.Break_Subprogram
              (To_String (B.Subprogram),
               Temporary => B.Disposition /= Keep, Mode => Internal);
         elsif B.Address /= Invalid_Address then
            Id := Process.Debugger.Break_Address
              (B.Address,
               Temporary => B.Disposition /= Keep, Mode => Internal);
         else
            Id := GVD.Types.No_Breakpoint;
         end if;

         if Id /= GVD.Types.No_Breakpoint then
            if not B.Enabled then
               Process.Debugger.Enable_Breakpoint (Id, B.Enabled, Internal);
            end if;

            if B.Condition /= "" then
               Process.Debugger.Set_Breakpoint_Condition
                 (Id, To_String (B.Condition), Internal);
            end if;

            if B.Commands /= "" then
               Process.Debugger.Set_Breakpoint_Command
                 (Id, To_String (B.Commands), Internal);
            end if;

            if B.Ignore /= 0 then
               Process.Debugger.Set_Breakpoint_Ignore_Count
                 (Id, B.Ignore, Internal);
            end if;

            if B.Scope /= No_Scope or else B.Action /= No_Action then
               Process.Debugger.Set_Scope_Action
                 (B.Scope, B.Action, Id, Internal);
            end if;
         end if;
      end loop;

      --  Reparse the list to make sure of what the debugger is actually using
      Refresh_Breakpoints_List (Kernel, Process);
   end Execute;

   -----------------------
   -- Toggle_Breakpoint --
   -----------------------

   function Toggle_Breakpoint
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Breakpoint_Num : Breakpoint_Identifier) return Boolean
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
   begin
      if Process /= null then
         for B of Process.Breakpoints.List loop
            if B.Num = Breakpoint_Num then
               B.Enabled := not B.Enabled;
               Process.Debugger.Enable_Breakpoint
                 (B.Num, B.Enabled, Mode => GVD.Types.Visible);
               return B.Enabled;
            end if;
         end loop;

      else
         for B of Module.Breakpoints.List loop
            if B.Num = Breakpoint_Num then
               B.Enabled := not B.Enabled;
               return B.Enabled;
            end if;
         end loop;
      end if;

      Show_Breakpoints_In_All_Editors (Kernel);
      return False;
   end Toggle_Breakpoint;

   ----------------------------
   -- Get_Breakpoint_From_Id --
   ----------------------------

   function Get_Breakpoint_From_Id
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Id      : Breakpoint_Identifier)
      return Breakpoint_Data
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));
   begin
      if Process /= null then
         for B of Process.Breakpoints.List loop
            if B.Num = Id then
               return B;
            end if;
         end loop;
      else
         for B of Module.Breakpoints.List loop
            if B.Num = Id then
               return B;
            end if;
         end loop;
      end if;
      return Null_Breakpoint;
   end Get_Breakpoint_From_Id;

   ------------------------------
   -- Refresh_Breakpoints_List --
   ------------------------------

   procedure Refresh_Breakpoints_List
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : not null access Base_Visual_Debugger'Class)
   is
      Process  : constant Visual_Debugger := Visual_Debugger (Debugger);
   begin
      if Process.Debugger = null
        or else Process.Command_In_Process
      then
         return;
      end if;

      Process.Debugger.List_Breakpoints (Process.Breakpoints.List);
      Process.Breakpoints.Has_Temporary_Breakpoint := False;

      for B of Process.Breakpoints.List loop

         --  Convert from a path returned by the debugger to the actual path in
         --  the project, in case sources have changed.

         if not B.File.Is_Regular_File then
            B.File := Kernel.Create_From_Base (B.File.Full_Name);
         end if;

         --  Check whether we have temporary breakpoints

         if B.Disposition /= Keep and then B.Enabled then
            Process.Breakpoints.Has_Temporary_Breakpoint := True;
         end if;
      end loop;

      Show_Breakpoints_In_All_Editors (Kernel);
      Debugger_Breakpoints_Changed_Hook.Run (Kernel, Process);
   end Refresh_Breakpoints_List;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Debugger_Location_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Debugger);
   begin
      Show_Breakpoints_In_All_Editors (Kernel);
   end Execute;

   ---------------------
   -- Add_Information --
   ---------------------

   procedure Add_Information
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Info    : in out Line_Information_Array;
      B       : Breakpoint_Data;
      Removed : Boolean) is
   begin
      if Removed then
         Info (B.Line).Image := Line_Might_Have_Code_Pixbuf;
      else
         if not B.Enabled then
            Info (B.Line).Image := Line_Has_Disabled_Breakpoint_Pixbuf;
            Info (B.Line).Tooltip_Text := To_Unbounded_String
              ("A disabled breakpoint has been set on this line");
         elsif B.Condition /= "" then
            Info (B.Line).Image := Line_Has_Conditional_Breakpoint_Pixbuf;
            Info (B.Line).Tooltip_Text := To_Unbounded_String
              ("A conditional breakpoint has been set on this line");
         else
            Info (B.Line).Image := Line_Has_Breakpoint_Pixbuf;
            Info (B.Line).Tooltip_Text := To_Unbounded_String
              ("An active breakpoint has been set on this line");
         end if;
      end if;

      Info (B.Line).Associated_Command := Create_Set_Breakpoint_Command
        (Kernel,
         Mode => (if Removed then Set else Unset),
         File => B.File,
         Line => B.Line);
   end Add_Information;

   -------------------------------------
   -- Show_Breakpoints_In_All_Editors --
   -------------------------------------

   procedure Show_Breakpoints_In_All_Editors
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      procedure Add (B : Breakpoint_Data);
      procedure Remove (B : Breakpoint_Data);
      --  Add or remove highlighting for a specific breakpoint

      procedure Process_List (List : Breakpoint_List);
      --  Display a specific list of breakpoints

      procedure Add (B : Breakpoint_Data) is
         A : Line_Information_Array (B.Line .. B.Line);
      begin
         Create_Breakpoints_Column (Kernel, B.File);
         Add_Information (Kernel, A, B, Removed => False);
         Add_Line_Information
           (Kernel,
            File       => B.File,
            Identifier => Breakpoints_Column_Id,
            Info       => A);
      end Add;

      procedure Remove (B : Breakpoint_Data) is
         A             : Line_Information_Array (B.Line .. B.Line);
      begin
         Add_Information (Kernel, A, B, Removed => True);
         Add_Line_Information
           (Kernel,
            File       => B.File,
            Identifier => Breakpoints_Column_Id,
            Info       => A);
      end Remove;

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List (List : Breakpoint_List) is
         To_Remove : array (Module.Editor_Breakpoints.List.First_Index
                            .. Module.Editor_Breakpoints.List.Last_Index)
           of Boolean := (others => True);

         Already_Visible : Boolean;
         Index           : Natural;
      begin
         --  Add the new breakpoints that were not already visible.
         --  Preserve those that are still visible

         for B of List.List loop
            if B.File /= No_File then
               Already_Visible := False;
               Index := To_Remove'First;
               for C of Module.Editor_Breakpoints.List loop
                  if C.Line = B.Line
                    and then C.File = B.File
                  then
                     Already_Visible := True;
                     To_Remove (Index) := False;
                     exit;
                  end if;
                  Index := Index + 1;
               end loop;

               if not Already_Visible then
                  Add (B);
               end if;
            end if;
         end loop;

         --  Now remove obsolete breakpoints

         for B in To_Remove'Range loop
            if To_Remove (B)
              and then Module.Editor_Breakpoints.List (B).File /= No_File
            then
               Remove (Module.Editor_Breakpoints.List (B));
            end if;
         end loop;

         --  Cache the list for the next refresh
         Module.Editor_Breakpoints := List;
      end Process_List;

   begin
      if Process /= null then
         Process_List (Process.Breakpoints);
      else
         Process_List (Module.Breakpoints);
      end if;
   end Show_Breakpoints_In_All_Editors;

   -------------------------------
   -- Create_Breakpoints_Column --
   -------------------------------

   procedure Create_Breakpoints_Column
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) is
   begin
      Create_Line_Information_Column
        (Kernel     => Kernel,
         Identifier => Breakpoints_Column_Id,
         File       => File);
   end Create_Breakpoints_Column;

   -------------------------------
   -- Remove_Breakpoints_Column --
   -------------------------------

   procedure Remove_Breakpoints_Column
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) is
   begin
      Remove_Line_Information_Column (Kernel, File, Breakpoints_Column_Id);
   end Remove_Breakpoints_Column;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_Lines_Revealed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Self);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Kernel));

      Line1, Line2 : Integer;

      procedure Process_List (List : Breakpoint_List);
      --  Process a specific list of breakpoints

      ------------------
      -- Process_List --
      ------------------

      procedure Process_List (List : Breakpoint_List) is
         type Bp_Array is array (Line1 .. Line2) of Integer;
         --  For each line of the file, the index of the breakpoint in
         --  List. Integer'First when there are no breakpoints

         File         : constant Virtual_File := File_Information (Context);
         A            : Line_Information_Array (Line1 .. Line2);
         Index        : Natural;
         Bps          : Bp_Array := (others => Integer'First);
         Br           : Breakpoint_Data;
         Has_Breakpoint : Boolean := False;

      begin
         --  Build an array of breakpoints in the current range, more
         --  efficient than re-browsing the whole array of breakpoints
         --  for each line.

         Index := List.List.First_Index;
         for B of List.List loop
            if B.Line in Bps'Range and then B.File = File then
               Bps (B.Line) := Index;
               Has_Breakpoint := True;
            end if;
            Index := Index + 1;
         end loop;

         --  ??? Should be whether there is any breakpoint in the file
         if True or else Has_Breakpoint then
            Create_Breakpoints_Column (Kernel, File);
         else
            Remove_Breakpoints_Column (Kernel, File);
         end if;

         for J in A'Range loop
            if Bps (J) /= Integer'First then
               Add_Information
                 (Kernel, A, List.List (Bps (J)), Removed => False);
            else
               Br.File := File;
               Br.Line := J;
               Add_Information (Kernel, A, Br, Removed => True);
            end if;
         end loop;

         Add_Line_Information
           (Kernel     => Kernel,
            Identifier => Breakpoints_Column_Id,
            File       => File,
            Info       => A);
      end Process_List;

   begin
      if not Has_Area_Information (Context) then
         return;
      end if;

      Get_Area (Context, Line1, Line2);

      if Process = null or else Process.Debugger = null then
         Process_List (Module.Breakpoints);
      elsif not Process.Command_In_Process then
         Process_List (Process.Breakpoints);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      No_Debugger_Or_Stopped : Action_Filter;
   begin
      Module := new Breakpoints_Module;
      Register_Module (Module, Kernel, "Persistent_Breakpoints");

      Project_Changed_Hook.Add (new On_Project_Changed);
      Project_Changing_Hook.Add (new On_Project_Changing);
      Before_Exit_Action_Hook.Add (new On_Before_Exit);
      Source_Lines_Revealed_Hook.Add (new On_Lines_Revealed);
      Debugger_Terminated_Hook.Add (new On_Debugger_Terminated);
      Debugger_Started_Hook.Add (new On_Debugger_Started);
      Debugger_Location_Changed_Hook.Add (new On_Debugger_Location_Changed);

      No_Debugger_Or_Stopped := new No_Debugger_Or_Stopped_Filter;

      Register_Action
        (Kernel, "debug set subprogram breakpoint",
         Command     => new Set_Breakpoint_Command_Context,
         Description => "Set a breakpoint on subprogram",
         Filter      => No_Debugger_Or_Stopped and
            new Subprogram_Variable_Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Set breakpoint on %e",
         Action => "debug set subprogram breakpoint");

      Register_Action
        (Kernel, "debug set line breakpoint",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => True, Continue_Till => False),
         Description => "Set a breakpoint on line",
         Filter      => No_Debugger_Or_Stopped and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Set breakpoint on line %l",
         Action => "debug set line breakpoint");

      Register_Action
        (Kernel, "debug continue until",
         Command     => new Set_Breakpoint_Command_Context'
           (Interactive_Command with On_Line => True, Continue_Till => True),
         Description => "Continue executing until the given line",
         Filter      => Kernel.Lookup_Filter ("Debugger stopped") and
           Kernel.Lookup_Filter ("Source editor"),
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Debug/Continue until line %l",
         Action => "debug continue until");
   end Register_Module;

end GVD.Breakpoints_List;
