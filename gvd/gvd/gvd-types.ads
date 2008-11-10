-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNATCOLL.VFS;
with GNAT.Strings;

package GVD.Types is

   type Command_Type is (Internal, Hidden, Visible, User);
   --  Internal commands are not stored into the command history. No output
   --  will be visible to the user.
   --  This is used for all the commands needed internally by gvd to update
   --  its windows, or for commands that are needed internally for higher
   --  level commands. These will not be saved in the session file.
   --
   --  Hidden commands are stored into the history but not shown in
   --  the debugger window. Their output is also not visible to the user.
   --
   --  Visible commands are shown into the command history and displayed in
   --  the debugger window, as well as their output. This is typically the
   --  mode to be used for menu items or toolbar buttons.
   --
   --  User is used for commands that have been typed manually by the user
   --  in the command window. These commands are inserted into the history,
   --  their output is visible in the command window, but the command itself
   --  is not printed

   subtype Invisible_Command is Command_Type range Internal .. Hidden;
   --  Invisible commands
   --  This type of command is always handled synchronousely

   subtype Visible_Command is Command_Type range Visible .. User;
   --  Visible commands
   --  This type of command is always handled asynchronousely

   ---------------
   -- Addresses --
   ---------------

   type Address_Type is private;
   --  An abstract representation of an address.

   Invalid_Address : constant Address_Type;
   --  Address_Type that represents an invalid address.

   function String_To_Address (Address_String : String) return Address_Type;
   --  Given  a string, return the corresponding Address_Type. If string
   --  does not represent a valid address, Invalid_Address is return.

   function Address_To_String (Address : Address_Type) return String;
   --  Return a string representation of Address.

   function Set_Offset
     (Address : Address_Type;
      Offset  : Natural) return Address_Type;

   overriding function "="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function ">"
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function ">="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function "<"
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function "<="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   --  Arithmetic on addresses

   -----------------
   -- Breakpoints --
   -----------------

   type Breakpoint_Type is (Breakpoint, Watchpoint);
   --  Types of breakpoints

   type Breakpoint_Disposition is (Delete, Disable, Keep);
   --  What to do with a breakpoint when it is reached.

   type Breakpoint_Identifier is new Natural;
   --  How breakpoints are identified. Currently, the debuggers supported
   --  by gvd all associate numbers with breakpoints.

   type VxWorks_Version_Type is (Vx_None, Vx5, Vx6, Vx653, Vx_Unknown);
   --  Used to keep track of the VxWorks version running on the target, as
   --  different versions have different debugging capabilities

   type Scope_Type is (Current_Task, Tasks_In_PD, Any_Task, No_Scope);
   type Action_Type is (Current_Task, Tasks_In_PD, All_Tasks, No_Action);
   --  For the VxWorks AE debugger, specify the "scope" (which tasks
   --  can hit a given breakpoint) and the "action" (which tasks to
   --  stop after a breakpoint has been hit) of a breakpoint.
   --
   --  Scope values:
   --  * Current_Task (task): only the running task
   --  * Tasks_In_PD (pd): all the tasks in a given protection domain
   --  * Any_Task (any): any task
   --
   --  Action values:
   --  * Current_Task (task): only the running task
   --  * Tasks_In_PD (pd): all the tasks in a given protection domain
   --  * All_Tasks (all): all breakable tasks
   --
   --  The No_Scope and No_Action value are provided as null values
   --
   --  These are the different combinations:
   --  scope        action     notes
   --   task         task       like monotask mode
   --   task         pd         stops all breakable tasks in current pd
   --   task         all        stops all breakable tasks
   --   pd           task
   --   pd           pd
   --   pd           all
   --   any          task       "any" only allowed in kernel domain
   --   any          pd         "any" only allowed in kernel domain
   --   any          all        "any" only allowed in kernel domain

   type Watchpoint_Trigger is (Read, Write, Read_Write);
   --  The kind of memory access that triggers a watchpoint

   type Breakpoint_Data is record
      Num         : Breakpoint_Identifier;
      --  breakpoint number (internal to the debugger)

      The_Type    : Breakpoint_Type;
      --  The type of breakpoint

      Disposition : Breakpoint_Disposition;
      --  What is done when the breakpoint is reached

      Enabled     : Boolean;
      --  Whether the breakpoint is currently enabled

      Address     : Address_Type;
      --  The address of the breakpoint.

      Trigger     : Watchpoint_Trigger;
      --  The action that causes the watchpoint to break the program.  The
      --  value set here is valid only for watchpoints.

      Expression  : GNAT.Strings.String_Access;
      --  The name of the variable to watch for watchpoints. This is left to
      --  null for breakpoints.

      File        : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      --  The file name that contains the breakpoint.
      --  Must be stored as an absolute file name.

      Except      : GNAT.Strings.String_Access;
      --  Name of the exception on which we break

      Subprogram  : GNAT.Strings.String_Access;
      --  Name of the subprogram we stop in.

      Line        : Integer := 0;
      --  The line that contains the breakpoint

      Ignore      : Natural := 0;
      --  Number of hits that will be ignored before actually stopping

      Condition   : GNAT.Strings.String_Access;
      --  Condition on which this breakpoint is activated

      Commands    : GNAT.Strings.String_Access;
      --  Commands to execute when the debugger stops at this breakpoint

      Scope       : Scope_Type;
      --  The scope of the breakpoint

      Action      : Action_Type;
      --  The action of the breakpoint
   end record;
   --  Information for a specific breakpoint

   type Breakpoint_Array is array (Natural range <>) of Breakpoint_Data;

   type Breakpoint_Array_Ptr is access Breakpoint_Array;

   procedure Free (Br : in out Breakpoint_Data);
   --  Free the memory allocated for a breakpoint data

   procedure Free (Br_Array : in out Breakpoint_Array);
   --  Free the memory allocated for a breakpoint array

   procedure Free (Br_Access : in out Breakpoint_Array_Ptr);
   --  Free the memory allocate for the array.

   ----------------
   -- Exceptions --
   ----------------

   type Exception_Data is record
      Name : GNAT.Strings.String_Access;
   end record;
   --  Description of an exception that can occur in the current application.

   type Exception_Array is array (Natural range <>) of Exception_Data;

   procedure Free (Exception_Access : in out Exception_Array);

   ------------------------
   -- Program_Descriptor --
   ------------------------

   type Launch_Method is (None, Current_Debugger, New_Debugger);

   type Debugger_Type is
     (Gdb_Type,
      VMS_Type,
      Dbx_Type,
      Xdb_Type,
      Jdb_Type,
      Pydb_Type,
      Perl_Type,
      Ladebug_Type);
   --  Type of debugger handled.
   --  Beware that some debuggers might not be available.

   type Program_Descriptor is record
      Program       : GNATCOLL.VFS.Virtual_File;
      Debugger      : Debugger_Type;
      Debugger_Name : GNAT.Strings.String_Access;
      Remote_Host   : GNAT.Strings.String_Access;
      Remote_Target : GNAT.Strings.String_Access;
      Protocol      : GNAT.Strings.String_Access;
      Launch        : Launch_Method;
   end record;
   --  This record contains all the information about how a debugger was
   --  started.
private
   subtype Address_Range is Integer range 0 .. 20;

   type Address_Type (Last : Address_Range := 0) is record
      Address_String : String (1 .. Last);
      --  The string representing the address

      Length         : Natural := 0;
      --  This is the length of the remaining string once the "0x" prefix as
      --  well as all the following zeros have been stripped.
      --  The meaningful part of Address_String is therefore the one in
      --  the Last - Length + 1 .. Last range.

      Offset         : Integer := 0;
      --  Offset used when the address is used to query the debugger.
   end record;

   Invalid_Address : constant Address_Type :=
                       (Address_String => "",
                        Last           => 0,
                        Length         => 0,
                        Offset         => 0);
end GVD.Types;
