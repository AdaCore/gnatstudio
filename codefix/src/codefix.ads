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
with Ada.Unchecked_Deallocation;

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

   ----------------------------------------------------------------------------
   --  type Dynamic_String
   ----------------------------------------------------------------------------

   --  All the functions of affectation (Affect and Get_Line) destroy the
   --  previous element in the Dynamic_String. If you don't want to delete
   --  the previous string, you have to initialize the Dynamic_String with
   --  the value null before calling these functions.

   type Dynamic_String is access all String;

   procedure Assign (This : in out Dynamic_String; Value : String);
   --  Delete the previous string, and create a new initialized with Value.

   procedure Assign (This : in out Dynamic_String; Value : Dynamic_String);
   --  Delete the previous string This, and create a new initialized with a
   --  copy of Value.

   procedure Get_Line (This : in out Dynamic_String);
   --  Delete the previous string, and create a new initialized with the line
   --  red on the standart input.

   procedure Get_Line (File : File_Type; This : in out Dynamic_String);
   --  Delete the previous string, and create a new initialized with the line
   --  red File.

   procedure Put_Line (This : Dynamic_String);
   --  Put the string referenced on the standart output.

   procedure Put_Line (File : File_Type; This : Dynamic_String);
   --  Put the string referenced on the file specified.

   procedure Free is new Ada.Unchecked_Deallocation (String, Dynamic_String);

   function Clone (This : Dynamic_String) return Dynamic_String;
   --  Duplicate all information contained in This, allocated in the pool.

end Codefix;
