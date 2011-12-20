------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Error;           use Glib.Error;
with Glib.Convert;         use Glib.Convert;
with Glib.Unicode;         use Glib.Unicode;

package body UTF8_Utils is

   ---------------------
   -- Unknown_To_UTF8 --
   ---------------------

   procedure Unknown_To_UTF8
     (Input   : String;
      Output  : out Unchecked_String_Access;
      Len     : out Natural;
      Success : out Boolean)
   is
      Valid       : Boolean;
      Invalid_Pos : Natural;
   begin
      Output := null;
      Len := 0;
      Success := True;

      --  First check if the string is already UTF-8
      UTF8_Validate (Input, Valid, Invalid_Pos);

      if Valid then
         --  The string is UTF-8, nothing to do
         return;

      else
         --  The string is not valid UTF-8, assume it is encoded using the
         --  locale.

         declare
            Tentative     : chars_ptr;
            Read, Written : aliased Natural;
            Error         : aliased GError := null;

         begin
            Tentative := Locale_To_UTF8
              (Input, Read'Access, Written'Access, Error'Unchecked_Access);

            if Error = null then
               --  There was no error in converting, return the converted
               --  string.

               Output := To_Unchecked_String (Tentative);
               Len := Written;

            else
               Error_Free (Error);
               --  ??? We could make some use of the error message

               --  Locale_To_UTF8 does not clarify whether Tentative is
               --  allocated some memory or not in case of failure. In doubt,
               --  check here.

               if Tentative /= Null_Ptr then
                  Free (Tentative);
               end if;

               --  We could not convert everything
               Success := False;
               return;
            end if;
         end;
      end if;
   end Unknown_To_UTF8;

   function Unknown_To_UTF8
     (Input   : String;
      Success : access Boolean) return Glib.UTF8_String
   is
      Output : Unchecked_String_Access;
      Len    : Natural;
   begin
      Unknown_To_UTF8 (Input, Output, Len, Success.all);

      if Success.all then
         if Len = 0 then
            return Input;
         else
            declare
               S : constant String := Output (1 .. Len);
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
     (Input : String) return Glib.UTF8_String
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

   function UTF8_To_Locale (Input : Glib.UTF8_String) return String is
      S : constant String := Locale_From_UTF8 (Input);
   begin
      if S = "" then
         return Input;
      else
         return S;
      end if;
   end UTF8_To_Locale;

end UTF8_Utils;
