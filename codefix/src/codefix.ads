-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This is the main package of Codefix, it define constants, exceptions and
--  tools that are used in others packages.

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Codefix is

   Codefix_Panic : exception;
   --  This exception is raised when a unpredicted error is made,
   --  most of times it is due to a bad usage of the functions by the
   --  programmer.

   Tab_Width : constant Natural := 8;
   --  Width of a tab in GNAT

   Indentation_Width : constant := 3;
   --  Width of an indentation in GNAT

   EOL_Str : constant String := (1 => ASCII.LF);
   --  String used to insert an end of line.

   Label_VDiff_Size_Limit : constant Natural := 50;
   --  Limits of the numbers of chars displayed in the VDiff's labels.

   --------------------------------
   -- String_Access manipulation --
   --------------------------------

   --  All the functions of affectation (Affect and Get_Line) destroy the
   --  previous element in the String_Access.

   procedure Assign (This : in out String_Access; Value : String);
   --  Delete the previous string, and create a new initialized with Value.

   procedure Assign (This : in out String_Access; Value : String_Access);
   --  Delete the previous string This, and create a new initialized with a
   --  copy of Value.

   procedure Get_Line (File : File_Type; This : in out String_Access);
   --  Delete the previous string, and create a new initialized with the line
   --  red File.

   function Clone (This : String_Access) return String_Access;
   --  Duplicate all information contained in This, allocated in the pool.

end Codefix;
