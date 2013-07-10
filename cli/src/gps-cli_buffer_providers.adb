------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Calendar;                 use Ada.Calendar;

with Basic_Types;                  use Basic_Types;
with Time_Utils;                   use Time_Utils;
with UTF8_Utils;                   use UTF8_Utils;

package body GPS.CLI_Buffer_Providers is

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Provider : access CLI_Buffer_Provider;
      File     : Virtual_File)
      return String_Access
   is
      pragma Unreferenced (Provider);
      Tmp     : String_Access;
      Tmp2    : Unchecked_String_Access;
      Len     : Natural;
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Tmp := Read_File (File);
      Unknown_To_UTF8 (Tmp.all, Tmp2, Len, Success);

      if Tmp2 /= null then
         Free (Tmp);
         Tmp := new String'(Tmp2 (1 .. Len));
         Free (Tmp2);
      end if;

      return Tmp;
   end Get_Buffer;

   -------------------
   -- Get_Timestamp --
   -------------------

   overriding function Get_Timestamp
     (Provider : access CLI_Buffer_Provider;
      File     : Virtual_File)
      return Integer
   is
      pragma Unreferenced (Provider);

      Stamp : Time;
      Y     : Year_Number;
      M     : Month_Number;
      D     : Day_Number;
      S     : Day_Duration;
   begin
      Stamp := File.File_Time_Stamp;

      Local_Split (Stamp, Y, M, D, S);

      return D * 86400 + Integer (S);
   end Get_Timestamp;

end GPS.CLI_Buffer_Providers;
