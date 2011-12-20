------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Environment_Variables;

package Templates_Parser.Utils is

   use Ada;

   function Image (N : Integer) return String;
   pragma Inline (Image);
   --  Returns N image without leading blank

   function Image (T : Tag) return String;
   --  Returns a string representation for this tag

   function Value (T : String) return Tag;
   --  Give a string representation of a tag (as encoded with Image above),
   --  build the corresponding Tag object. Raises Constraint_Error if T is
   --  not a valid tag representation.

   function Get_Program_Directory return String;
   --  Returns the directory full path name for the current running program

   Is_Windows          : constant Boolean :=
                           Environment_Variables.Exists ("OS") and then
                           Environment_Variables.Value ("OS") = "Windows_NT";

   Directory_Separator : constant Character;
   Path_Separator      : constant Character;

   function Executable_Extension return String;
   --  Return the executable exetension for the running host

   function Web_Escape (S : String) return String;
   --  Encode all characters that cannot be used as-is into an HTML page

   function Is_Number (S : String) return Boolean;
   --  Returns true if S is composed of digits only

private

   subtype Windows_Host is Boolean;

   type C_Array is array (Windows_Host) of Character;

   DS : C_Array := C_Array'(True => '\', False => '/');
   PS : C_Array := C_Array'(True => ';', False => ':');

   Directory_Separator : constant Character := DS (Is_Windows);
   Path_Separator      : constant Character := PS (Is_Windows);

end Templates_Parser.Utils;
