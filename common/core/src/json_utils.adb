------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

package body JSON_Utils is

   ----------
   -- Load --
   ----------

   function Load (Value : JSON_Value) return Virtual_File is
   begin
      if String'(Value.Get ("file")) = "" then
         return No_File;
      else
         declare
            Host : constant String := Value.Get ("server");
         begin
            if Host = "" then
               return Create (+Value.Get ("file"));
            else
               return Create (+Value.Get ("file"), Host);
            end if;
         end;
      end if;
   end Load;

   ----------
   -- Save --
   ----------

   function Save (File : Virtual_File) return JSON_Value is
      Value : constant JSON_Value := Create_Object;
   begin
      if File /= GNATCOLL.VFS.No_File then
         Value.Set_Field ("file", +Full_Name (File));

         declare
            Host : constant String := Get_Host (File);
         begin
            if Host /= Local_Host then
               Value.Set_Field ("server", Host);
            else
               Value.Set_Field ("server", "");
            end if;
         end;
      else
         Value.Set_Field ("file", "");
      end if;

      return Value;
   end Save;

end JSON_Utils;
