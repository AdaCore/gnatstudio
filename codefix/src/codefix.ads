-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2008, AdaCore               --
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
with GNAT.Strings; use GNAT.Strings;

with Generic_List;

package Codefix is

   Codefix_Panic : exception;
   --  This exception is raised when a unpredicted error is made,
   --  most of times it is due to a bad usage of the functions by the
   --  programmer.

   Obsolescent_Fix : exception;
   --  This exception is raised when a fix is no longer valid - this may be
   --  due to changes in the text between the time the fix is computed and
   --  when the fix is applied. This exception is expected to be raised by
   --  the fix functions, and should not issue an unexpected exception trace.

   Tab_Width : constant Natural := 8;
   --  Width of a tab in GNAT

   Indentation_Width : constant := 3;
   --  Width of an indentation in GNAT

   EOL_Str : constant String := (1 => ASCII.LF);
   --  String used to insert an end of line.

   Label_VDiff_Size_Limit : constant Natural := 50;
   --  Limits of the numbers of chars displayed in the VDiff's labels.

   type Column_Index is new Natural;
   --  Type of a column
   --  ??? This should either be a Visible_Column_Type or
   --  a Character_Offset_Type.

   type Char_Index is new Natural;
   --  Type of an index in a string

   function To_Char_Index
     (Index : Column_Index; Str : String) return Char_Index;
   --  Return the char position corresponding to the column given in parameter
   --  This will handle tabulations

   function To_Column_Index
     (Index : Char_Index; Str : String) return Column_Index;
   --  Return the column index corresponding to the char index given in
   --  parameter. This will handle tablulations.

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

   ----------------------------------------------------------------------------
   --  type Useless_Entity_Operation
   ----------------------------------------------------------------------------

   type Useless_Entity_Operations is mod 8;

   Nothing                 : constant Useless_Entity_Operations;
   Remove_Entity           : constant Useless_Entity_Operations;
   Add_Pragma_Unreferenced : constant Useless_Entity_Operations;
   Comment_Entity          : constant Useless_Entity_Operations;

   function Is_Set
     (Mask : Useless_Entity_Operations;
      Flag : Useless_Entity_Operations) return Boolean;
   --  Returns true if the Flag is contained in the Mask.

   type Fix_Options is record
      Remove_Policy : Useless_Entity_Operations := Remove_Entity;
   end record;
   --  This record hold various options used to configure the fix.

   ----------------------------------------------------------------------------
   --  type Error_State
   ----------------------------------------------------------------------------

   type Error_State is (Enabled, Disabled, Unknown);
   --  The two states possible for an error.

   type State_List is private;

   procedure Set_Error_State
     (List : in out State_List; Error : String; State : Error_State);
   --  Modify the current error state.

   function Get_Error_State
     (List : State_List; Error : String) return Error_State;
   --  Return the current error state.

private

   Nothing                 : constant Useless_Entity_Operations := 0;
   Remove_Entity           : constant Useless_Entity_Operations := 1;
   Add_Pragma_Unreferenced : constant Useless_Entity_Operations := 2;
   Comment_Entity          : constant Useless_Entity_Operations := 4;

   type State_Node is record
      Error : GNAT.Strings.String_Access;
      State : Error_State := Unknown;
   end record;

   procedure Free (This : in out State_Node);

   package State_Lists is new Generic_List (State_Node);
   use State_Lists;

   type State_List is new State_Lists.List;

end Codefix;
