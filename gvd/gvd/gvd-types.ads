-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Basic_Types; use Basic_Types;

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

   type Breakpoint_Data is record
      Num         : Breakpoint_Identifier;
      --  breakpoint number (internal to the debugger)

      The_Type    : Breakpoint_Type;
      --  The type of breakpoint

      Disposition : Breakpoint_Disposition;
      --  What is done when the breakpoint is reached

      Enabled     : Boolean;
      --  Whether the breakpoint is currently enabled

      Address     : String_Access;
      --  The address of the breakpoint.

      Expression  : String_Access;
      --  The name of the variable to watch for watchpoints. This is left to
      --  null for breakpoints.

      File        : String_Access;
      --  The file name that contains the breakpoint

      Except      : String_Access;
      --  Name of the exception on which we break

      Subprogram  : String_Access;
      --  Name of the subprogram we stop in.

      Line        : Integer;
      --  The line that contains the breakpoint

      Info        : String_Access;
      --  Additionnal information

      Ignore      : Natural := 0;
      --  Number of hits that will be ignored before actually stoping

      Condition   : String_Access;
      --  Condition on which this breakpoint is activated

      Commands    : String_Access;
      --  Commands to execute when the debugger stops at this breakpoint
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
      Name : String_Access;
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
      Dbx_Type,
      Xdb_Type,
      Jdb_Type,
      Pydb_Type,
      Perl_Type,
      Ladebug_Type);
   --  Type of debugger handled.
   --  Beware that some debuggers might not be available.

   type Program_Descriptor is record
      Program       : String_Access;
      Debugger      : Debugger_Type;
      Debugger_Name : String_Access;
      Remote_Host   : String_Access;
      Remote_Target : String_Access;
      Protocol      : String_Access;
      Launch        : Launch_Method;
   end record;
   --  This record contains all the information about how a debugger was
   --  started.

end GVD.Types;
