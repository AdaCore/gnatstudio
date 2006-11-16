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

with Glib.Xml_Int; use Glib.Xml_Int;

package body XML_Parsers is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (File  : String;
      Tree  : out Glib.Xml_Int.Node_Ptr;
      Error : out GNAT.Strings.String_Access)
   is
   begin
      Tree := Glib.Xml_Int.Parse (File);
      if Tree = null then
         Error := new String'("Error while parsing " & File);
      else
         Error := null;
      end if;
   end Parse;

   ------------------
   -- Parse_Buffer --
   ------------------

   procedure Parse_Buffer
     (Buffer     : Glib.UTF8_String;
      From_File  : String := "<input>";
      Start_Line : Natural := 1;
      Tree       : out Glib.Xml_Int.Node_Ptr;
      Error      : out GNAT.Strings.String_Access)
   is
      pragma Unreferenced (From_File, Start_Line);
   begin
      Tree := Glib.Xml_Int.Parse_Buffer (Buffer);
      if Tree = null then
         Error := new String'
           ("Error while parsing custom string: " & Buffer);
      else
         Error := null;
      end if;
   end Parse_Buffer;
end XML_Parsers;
