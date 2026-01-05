------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

with GNATCOLL.Utils;            use GNATCOLL.Utils;
with String_Utils;              use String_Utils;
with GNAT.OS_Lib;
with UTF8_Utils;                use UTF8_Utils;

package body File_Utils is

   -----------------------
   -- URL_List_To_Files --
   -----------------------

   function URL_List_To_Files (URL_List : String) return File_Array_Access is

      Files  : constant String := Strip_CR (URL_List);
      File   : Virtual_File;
      First  : Natural := Files'First;
      Last   : Natural := First;
      Result : File_Array_Access;
   begin
      while First <= Files'Last loop
         String_Utils.Skip_To_Char (Files, Last, ASCII.LF);

         if First + 7 < Last
           and then Files (First .. First + 7) = "file:///"
         then
            --  if File in form like 'file:///C:/path'
            if First + 9 < Last and then Files (First + 9) = ':' then
               --  return C:/path
               File := Create
                 (+Locale_To_UTF8 (GNAT.OS_Lib.Normalize_Pathname
                  (URL_Decode (Files (First + 8 .. Last - 1)))));
            else
               --  otherwise get leading '/' into file name
               File := Create
                 (+Locale_To_UTF8 (GNAT.OS_Lib.Normalize_Pathname
                  (URL_Decode (Files (First + 7 .. Last - 1)))));
            end if;

            Append (Result, File);
         end if;

         First := Last + 1;
         Last  := First;
      end loop;

      return Result;
   end URL_List_To_Files;

   --------------------
   -- UTF8_Full_Name --
   --------------------

   function UTF8_Full_Name (File : Virtual_File) return UTF8_String is
      Ok     : aliased Boolean;
      Image  : constant String := Display_Full_Name (File);
      Result : constant UTF8_String := Unknown_To_UTF8 (Image, Ok'Access);
   begin
      if Ok then
         return Result;
      else
         --  Convert file name as it would Latin-1 string
         return Latin_1_To_UTF8 (Image);
      end if;
   end UTF8_Full_Name;

end File_Utils;
