------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with GNAT.Strings;
with Ada.Unchecked_Deallocation;

package Line_Buffers is

   --------------------------
   -- Line Buffer Handling --
   --------------------------

   --  The line buffer represents a buffer (e.g contents of a file) line
   --  by line. Line separators (LF or CR/LF) are kept at the end of the buffer
   --  It is recommended to take advantage of the bound information that comes
   --  with a String_Access so that there can be a direct mapping between
   --  the original raw buffer and a line buffer.
   --  Len is used to keep the length of the original line stored. Since this
   --  type is intended for making changes in buffers at a minimal cost
   --  (e.g avoiding copies of complete buffers when inserting a few
   --  characters), being able to convert from the original buffer's position
   --  information to the line buffer is critical and is achieved using the Len
   --  field.

   type Line_Buffer_Record;
   type Line_Buffer is access Line_Buffer_Record;
   type Line_Buffer_Record is record
      Line : GNAT.Strings.String_Access;
      Next : Line_Buffer;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Line_Buffer_Record, Line_Buffer);

   type Extended_Line_Buffer is record
      First        : Line_Buffer;
      Current      : Line_Buffer;
      Current_Line : Natural := 1;
   end record;

   function To_Line_Buffer (Buffer : String) return Extended_Line_Buffer;
   --  Convert a string to a line buffer.
   --  CR/LF and LF are treated as end of lines.

   procedure Print (Buffer : Extended_Line_Buffer);
   --  Output the contents of Buffer on standard output.

   procedure Free (Buffer : in out Extended_Line_Buffer);
   --  Free the contents of buffer.

   procedure Replace_Text
     (Buffer  : in out Extended_Line_Buffer;
      Line    : Natural;
      First   : Natural;
      Last    : Natural;
      Replace : String);
   --  Replace the columns First .. Last - 1 at line Line in Buffer by Replace.

end Line_Buffers;
