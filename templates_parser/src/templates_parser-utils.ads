------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2010, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
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
