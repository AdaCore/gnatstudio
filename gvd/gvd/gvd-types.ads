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

end Odd.Types;
