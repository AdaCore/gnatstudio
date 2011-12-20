------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with GNAT.Strings;   use GNAT.Strings;
with Traces;         use Traces;

with Glib.Convert;

package body Language_Utils is

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
           (Lang, Glib.Convert.Locale_To_UTF8 (Buffer.all), Result);
         Free (Buffer);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Free (Buffer);
   end Parse_File_Constructs;

end Language_Utils;
