------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with GNATCOLL.Iconv;       use GNATCOLL.Iconv;
with GNAT.Decode_UTF8_String;
with Config;

package body UTF8_Utils is

   Locale_To_UTF_8 : Iconv_T;
   UTF_8_To_Locale : Iconv_T;
   Is_Opened       : Boolean := False;

   procedure Open;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      if not Is_Opened then
         Locale_To_UTF_8 := Iconv_Open
           (To_Code => UTF8, From_Code => Config.Default_Charset);
         UTF_8_To_Locale := Iconv_Open
           (From_Code => UTF8, To_Code => Config.Default_Charset);
         Is_Opened := True;
      end if;
   end Open;

   ---------------------
   -- Unknown_To_UTF8 --
   ---------------------

   procedure Unknown_To_UTF8
     (Input   : String;
      Output  : out String_Access;
      Success : out Boolean) is
   begin
      Output := null;
      Success := True;

      --  First check if the string is already UTF-8
      if GNAT.Decode_UTF8_String.Validate_Wide_Wide_String (Input) then
         --  The string is UTF-8, nothing to do
         return;
      end if;

      --  The string is not valid UTF-8, assume it is encoded using the locale.

      begin
         Open;
         Output := new String'(Iconv (Locale_To_UTF_8, Input));
      exception
         when Invalid_Sequence_Error | Incomplete_Sequence_Error =>
            Reset (Locale_To_UTF_8);
            Success := False;
      end;
   end Unknown_To_UTF8;

   function Unknown_To_UTF8
     (Input   : String;
      Success : access Boolean) return UTF8_String
   is
      Output : String_Access;
   begin

      Unknown_To_UTF8 (Input, Output, Success.all);

      if Success.all then
         if Output = null then
            return Input;
         else
            declare
               S : constant String := Output.all;
            begin
               Free (Output);
               return S;
            end;
         end if;

      else
         return "";
      end if;
   end Unknown_To_UTF8;

   function Unknown_To_UTF8
     (Input : String) return UTF8_String
   is
      Success : aliased Boolean;
      S       : constant String := Unknown_To_UTF8 (Input, Success'Access);
   begin
      if Success then
         return S;
      else
         return "<could not convert to UTF8>";
      end if;
   end Unknown_To_UTF8;

   --------------------
   -- UTF8_To_Locale --
   --------------------

   function UTF8_To_Locale (Input : UTF8_String) return String is
   begin
      Open;
      return Iconv (UTF_8_To_Locale, Input);
   end UTF8_To_Locale;

end UTF8_Utils;
