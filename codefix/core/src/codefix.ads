------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

--  This is the main package of Codefix, it define constants, exceptions and
--  tools that are used in others packages.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Basic_Types;           use Basic_Types;

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

   EOL_Str : constant String := (1 => ASCII.LF);
   --  String used to insert an end of line.

   function To_Char_Index
     (Index : Visible_Column_Type; Str : String) return String_Index_Type
     with Post => To_Char_Index'Result <= String_Index_Type (Str'Last + 1);
   function To_Char_Index
     (Index : Visible_Column_Type;
      Str   : Unbounded_String) return String_Index_Type;
   --  Return the char position corresponding to the column given in parameter
   --  This will handle tabulations

   function To_Column_Index
     (Index : String_Index_Type; Str : String) return Visible_Column_Type;
   function To_Column_Index
     (Index : String_Index_Type;
      Str   : Unbounded_String) return Visible_Column_Type;
   --  Return the column index corresponding to the char index given in
   --  parameter. This will handle tabulations.

   type Root_Error_Parser is abstract tagged null record;
   --  Root type for all error parsers

   type Error_Parser_Access is access all Root_Error_Parser'Class;

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

   type Codefix_Remove_Policy is
     (Always_Remove, Always_Comment, Propose_Both_Choices);
   --  The list of possible remove policy when fixing code preference.

   function Policy_To_Operations
     (Policy : Codefix_Remove_Policy) return Useless_Entity_Operations;
   --  Return the Remove and/or Comment flag associated with this Policy

   type Fix_Options is record
      Remove_Policy : Useless_Entity_Operations := Remove_Entity;
   end record;
   --  This record hold various options used to configure the fix.

private

   Nothing                 : constant Useless_Entity_Operations := 0;
   Remove_Entity           : constant Useless_Entity_Operations := 1;
   Add_Pragma_Unreferenced : constant Useless_Entity_Operations := 2;
   Comment_Entity          : constant Useless_Entity_Operations := 4;

end Codefix;
