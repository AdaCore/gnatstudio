-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gtkada.Types;
with Unchecked_Deallocation;

package Odd.Types is

   subtype Pixmap_Array is Gtkada.Types.Chars_Ptr_Array (0 .. 0);
   type Pixmap_Access is access all Pixmap_Array;

   type String_Access is access all String;
   procedure Free is new Unchecked_Deallocation (String, String_Access);

   type String_Array is array (Natural range <>) of String_Access;
   procedure Free (Ar : in out String_Array);
   --  Free all the strings in the array.

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

   -----------------
   -- Breakpoints --
   -----------------

   type Breakpoint_Type is (Breakpoint, Watchpoint);
   --  Types of breakpoints

   type Breakpoint_Disposition is (Delete, Disable, Keep);
   --  What to do with a breakpoint when it is reached.

   type Breakpoint_Data is record
      Num         : Natural;
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

   -----------------
   -- File caches --
   -----------------

   type Packed_Boolean_Array is array (Positive range <>) of Boolean;
   pragma Pack (Packed_Boolean_Array);
   type Packed_Boolean_Access is access Packed_Boolean_Array;

   procedure Free is new Unchecked_Deallocation
     (Packed_Boolean_Array, Packed_Boolean_Access);

   type File_Cache;
   type File_Cache_List is access File_Cache;
   type File_Cache is record
      File_Name     : String_Access := null;
      --  The full name (including directory) for the file associated with
      --  this record.

      Line_Has_Code : Packed_Boolean_Access := null;
      Line_Parsed   : Packed_Boolean_Access := null;

      Current_Line  : Natural := 0;
      --  Last line that was parsed. No line before that one will be tested
      --  any more.

      File_Contents : String_Access := null;
      --  The contents of the file. To save some memory, this is not allocated
      --  for files that can be found on the local disk. However, it is used
      --  for files that had to be downloaded from a remote machine.

      Next : File_Cache_List := null;
      --  Next file in the cache list
   end record;
   --  Data associated with each file, and that contain cached data for the
   --  file.
   --  Line_Parsed indicates whether the line at a given index has been parsed.
   --  This array is freed once the parsing has been finished (and in the
   --  case Current_Line points to the last line with a breakpoint.

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

end Odd.Types;
