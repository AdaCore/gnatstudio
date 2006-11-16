-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2006                       --
--                             AdaCore                               --
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

with Glib.Xml_Int;
with GNAT.Strings;

package XML_Parsers is

   procedure Parse
     (File  : String;
      Tree  : out Glib.Xml_Int.Node_Ptr;
      Error : out GNAT.Strings.String_Access);
   --  Parse File, and return a pointer to the resulting Tree.
   --  The resulting value must be freed by the user

   procedure Parse_Buffer
     (Buffer     : Glib.UTF8_String;
      From_File  : String  := "<input>";
      Start_Line : Natural := 1;
      Tree       : out Glib.Xml_Int.Node_Ptr;
      Error      : out GNAT.Strings.String_Access);
   --  Same as above, but the XML string is already in memory.
   --  (From_File, Start_Line) indicate where the buffer was read from. This
   --  is in particular useful when reading strings in script files through
   --  calls to parse_xml()

end XML_Parsers;
