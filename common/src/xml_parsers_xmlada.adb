-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2003-2009, AdaCore                 --
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

with Unicode.CES;
with XML_Readers; use XML_Readers;

package body XML_Parsers is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (File  : Virtual_File;
      Tree  : out XML_Utils.Node_Ptr;
      Error : out GNAT.Strings.String_Access)
   is
      Err : Unicode.CES.Byte_Sequence_Access;
   begin
      Parse (File, Tree, Err);
      Error := GNAT.Strings.String_Access (Err);
   end Parse;

   ------------------
   -- Parse_Buffer --
   ------------------

   procedure Parse_Buffer
     (Buffer     : String;
      From_File  : Filesystem_String := "<input>";
      Start_Line : Natural := 1;
      Tree       : out XML_Utils.Node_Ptr;
      Error      : out GNAT.Strings.String_Access)
   is
      Err : Unicode.CES.Byte_Sequence_Access;
   begin
      Parse_Buffer (Buffer, Tree, Err, +From_File, Start_Line);
      Error := GNAT.Strings.String_Access (Err);
   end Parse_Buffer;

end XML_Parsers;
