------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Basic_Types;            use Basic_Types;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GVD.Types;              use GVD.Types;
with GPS.Debuggers;          use GPS.Debuggers;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Markers;            use GPS.Markers;

package GVD.Breakpoints_List is

   type Breakpoint_Type is (Breakpoint, Watchpoint, Catchpoint, Other);
   --  Types of breakpoints

   type Breakpoint_Disposition is (Delete, Disable, Keep);
   --  What to do with a breakpoint when it is reached.

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Setup the loading and saving of persistent breakpoints across GPS
   --  sessions. These breakpoints are associated with the root project, and
   --  are automatically loaded and saved when the project changes.
   --
   --  This procedure also automatically monitors changes in the list of
   --  breakpoints for the current debugger (if there is one).

   procedure Refresh_Breakpoints_List
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Reload the list of breakpoints from the running debugger.
   --  This runs the Debugger_Breakpoints_Changed_Hook.

   procedure Set_Breakpoints_State
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List;
      State  : Boolean);
   --  Use State to set the state of each breakpoint of the list.

   procedure Break_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Editable_Line_Type;
      Temporary     : Boolean := False);
   procedure Unbreak_Source
     (Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Line          : Editable_Line_Type);
   --  Set a breakpoint on the given location.
   --  If no debugger is currently running, the breakpoint will be applied when
   --  one is started. If one or more debuggers are running, they all break
   --  at that location

   procedure Break_Subprogram
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Subprogram    : String;
      Temporary     : Boolean := False);
   --  Set a breakpoint on the given subprogram.
   --  If no debugger is currently running, the breakpoint will be applied when
   --  one is started. If one or more debuggers are running, they all break
   --  on that subprogram.

   procedure Delete_Multiple_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List);
   --  Go through the list and delete the breakpoints. The list is not freed
   --  by this procedure.

   procedure Clear_All_Breakpoints
     (Kernel        : not null access Kernel_Handle_Record'Class);
   --  Remove all breakpoints

   type Breakpoint_Data is record
      Num         : Breakpoint_Identifier := GVD.Types.No_Breakpoint;
      --  breakpoint number (internal to the debugger)

      The_Type      : Breakpoint_Type := Breakpoint;
      --  The type of breakpoint
      The_Type_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  The name of the breakpoint's type if the type is Other

      Disposition : Breakpoint_Disposition := Keep;
      --  What is done when the breakpoint is reached

      Enabled     : Boolean := True;
      --  Whether the breakpoint is currently enabled

      Address     : Address_Type := Invalid_Address;
      --  The address of the breakpoint.

      Trigger     : Watchpoint_Trigger := Write;
      --  The action that causes the watchpoint to break the program.  The
      --  value set here is valid only for watchpoints.

      Expression  : Unbounded_String;
      --  The name of the variable to watch for watchpoints. This is left to
      --  null for breakpoints.

      Except      : Unbounded_String;
      --  Name of the exception on which we break

      Subprogram  : Unbounded_String;
      --  Name of the subprogram we stop in.

      Location    : Location_Marker := No_Marker;
      --  The location of the breakpoint

      Condition   : Unbounded_String;
      --  Condition on which this breakpoint is activated

      Ignore      : Natural := 0;
      --  Number of hits that will be ignored before actually stopping

      Commands    : Unbounded_String;
      --  Commands to execute when the debugger stops at this breakpoint

      Scope       : Scope_Type := No_Scope;
      --  The scope of the breakpoint

      Action      : Action_Type := No_Action;
      --  The action of the breakpoint
   end record;
   --  Information for a specific breakpoint

   function Is_Equal (B1, B2 : Breakpoint_Data) return Boolean;

   overriding function "=" (B1, B2 : Breakpoint_Data) return Boolean
     is (B1.Num = B2.Num);
   --  Whether the two breakpoints are the same.

   Null_Breakpoint : constant Breakpoint_Data;

   package Breakpoint_Vectors is new Ada.Containers.Vectors
     (Positive, Breakpoint_Data);

   type Breakpoint_List is record
      List                     : Breakpoint_Vectors.Vector;
      Has_Temporary_Breakpoint : Boolean := False;

      Dummy_Id                 : Breakpoint_Identifier := 1;
      --  To be able to assign dummy identifiers to breakpoints created before
      --  the debugger starts. These are used to manipulate them in the
      --  Breakpoints view.
   end record;

   function Get_Breakpoint_From_Id
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Id      : Breakpoint_Identifier)
      return Breakpoint_Data;
   --  Return the breakpint with the given identifier, or Null_Breakpoint

   function Get_Stored_List_Of_Breakpoints
     (Debugger : access Base_Visual_Debugger'Class := null)
     return access Breakpoint_List;
   --  Return the list of breakpoints.
   --  If Debugger is specified, this is the list of breakpoints specific to
   --  that debugger. Otherwise, this is the global list of persistent
   --  breakpoints.

   procedure Show_Breakpoints_In_All_Editors
     (Kernel                    : not null access Kernel_Handle_Record'Class;
      Show_Debugger_Breakpoints : Boolean := True);
   --  Update the side column for all editors, and show the breakpoints info
   --
   --  When Show_Debugger_Breakpoints is True, the breakpoints shown in the
   --  editors will be the ones stored for the currently used debugger, if any.
   --  Otherwise, it's the persistant ones that will be shown.

private
   Null_Breakpoint : constant Breakpoint_Data :=
     (Num => No_Breakpoint, others => <>);

end GVD.Breakpoints_List;
