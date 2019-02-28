------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GNAT.Strings;    use GNAT.Strings;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with UTF8_Utils;      use UTF8_Utils;

package body Language_Utils is
   Me : constant Trace_Handle := Create ("LANGUAGES");

   ---------------------------
   -- Parse_File_Constructs --
   ---------------------------

   procedure Parse_File_Constructs
     (Lang      : access Language_Root'Class;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Result    : out Construct_List)
   is
      Buffer : GNAT.Strings.String_Access;
   begin
      Buffer := GNATCOLL.VFS.Read_File (File_Name);

      if Buffer /= null then
         --  ??? The call to Locale_To_UTF8 is not optimal
         Parse_Constructs
           (Lang, File_Name, Locale_To_UTF8 (Buffer.all), Result);
         Free (Buffer);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Free (Buffer);
   end Parse_File_Constructs;

end Language_Utils;
