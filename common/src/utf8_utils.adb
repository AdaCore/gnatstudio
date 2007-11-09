-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007, AdaCore                  --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Error;                use Glib.Error;
with Glib.Convert;              use Glib.Convert;
with Glib.Unicode;              use Glib.Unicode;

with Interfaces.C.Strings;      use Interfaces.C.Strings;

with Ada.Unchecked_Deallocation;

package body UTF8_Utils is

   ---------------------
   -- Unknown_To_UTF8 --
   ---------------------

   function Unknown_To_UTF8
     (Input   : String;
      Success : access Boolean)
      return Glib.UTF8_String
   is
      Valid       : Boolean;
      Invalid_Pos : Natural;
   begin
      Success.all := True;

      --  First check if the string is already UTF-8
      UTF8_Validate (Input, Valid, Invalid_Pos);

      if Valid then
         --  The string is UTF-8, return it.
         return Input;
      else
         --  The string is not valid UTF-8, assume it is encoded using the
         --  locale.

         declare
            Tentative     : chars_ptr;
            Read, Written : aliased Natural;
            Error         : GError_Access := new GError'(null);
            procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (GError, GError_Access);
         begin
            Tentative := Locale_To_UTF8
              (Input, Read'Access, Written'Access, Error);

            if Error.all = null then
               --  There was no error in converting, return the converted
               --  string.
               Unchecked_Free (Error);

               declare
                  Result : constant String := Value (Tentative);
               begin
                  Free (Tentative);
                  return Result;
               end;
            else
               Error_Free (Error.all);
               --  ??? We could make some use of the error message.

               --  Locale_To_UTF8 does not clarify whether Tentative is
               --  allocated some memory or not in case of failure. In doubt,
               --  check here.

               if Tentative /= Null_Ptr then
                  Free (Tentative);
               end if;

               Unchecked_Free (Error);

               --  We could not convert everything; return an empty string.
               Success.all := False;
               return "";
            end if;
         end;
      end if;
   end Unknown_To_UTF8;

end UTF8_Utils;
