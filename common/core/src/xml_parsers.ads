------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with XML_Utils;
with GNAT.Strings;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package XML_Parsers is

   procedure Parse
     (File  : Virtual_File;
      Tree  : out XML_Utils.Node_Ptr;
      Error : out GNAT.Strings.String_Access);
   --  Parse File, and return a pointer to the resulting Tree.
   --  The resulting value must be freed by the user

   procedure Parse_Buffer
     (Buffer     : String;
      From_File  : Filesystem_String  := "<input>";
      Start_Line : Natural := 1;
      Tree       : out XML_Utils.Node_Ptr;
      Error      : out GNAT.Strings.String_Access);
   --  Same as above, but the XML string is already in memory.
   --  (From_File, Start_Line) indicate where the buffer was read from. This
   --  is in particular useful when reading strings in script files through
   --  calls to parse_xml()

end XML_Parsers;
