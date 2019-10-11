------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

package GNATdoc.Text_Buffers is

   type Text_Buffer is tagged private;

   function Is_Empty (Self : Text_Buffer'Class) return Boolean;
   --  Return True when buffer is empty.

   function Text
     (Self : Text_Buffer'Class) return Unbounded_String_Vectors.Vector;
   --  Return accumulated text.

   function To_Text_Buffer
     (Item : Unbounded_String_Vectors.Vector) return Text_Buffer;
   --  Create text buffer with given content

   procedure Append
     (Self : in out Text_Buffer'Class;
      Item : Unbounded_String);
   --  Append string to the last line.

   procedure Append_Line
     (Self : in out Text_Buffer'Class;
      Item : Unbounded_String);
   --  Append string to the last line and starts new line.

   procedure Append_Line
     (Self : in out Text_Buffer'Class;
      Item : Unbounded_String_Vectors.Vector);
   --  Append list of string to the text buffer. First string of the list
   --  will be merged to the last line when buffer is not at the end of line.

private

   type Text_Buffer is tagged record
      Text     : Unbounded_String_Vectors.Vector;
      New_Line : Boolean := True;
      --  Virtual new line character, used to preserve lines when adding
      --  text segments to the latest line.
   end record;

end GNATdoc.Text_Buffers;
