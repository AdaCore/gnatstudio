------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with VSS.Strings.Conversions;

package body DAP.Modules.Variables.Items is

   --------------
   --  Convert --
   --------------

   function Convert (Format : DAP.Tools.ValueFormat) return Value_Format is
   begin
      if Format = Default_Format then
         return Default;
      else
         return Hexadecimal;
      end if;
   end Convert;

   --------------
   --  Convert --
   --------------

   function Convert (Format : Value_Format) return DAP.Tools.ValueFormat is
   begin
      if Format = Default then
         return Default_Format;
      else
         return DAP.Tools.ValueFormat'(hex => True);
      end if;
   end Convert;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Item_Info) return Virtual_String is
   begin
      if Self.Varname /= "" then
         return Self.Varname;

      elsif Self.Cmd_Name /= ""
        and then Self.Cmd_Name /= "<>"
      then
         return Self.Cmd_Name;

      else
         return Self.Cmd;
      end if;
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Item_Info) return String is
   begin
      return VSS.Strings.Conversions.To_UTF_8_String (Get_Name (Self));
   end Get_Name;

   -------------
   -- Is_Same --
   -------------

   function Is_Same (Info : Item_Info; Name : Virtual_String) return Boolean is
   begin
      return Info.Cmd = Name or else Info.Varname = Name;
   end Is_Same;

   -----------
   -- Image --
   -----------

   function Image (Format : DAP.Tools.ValueFormat) return String is
   begin
      if Format = Default_Format then
         return "";
      else
         return " (Hexadecimal)";
      end if;
   end Image;

end DAP.Modules.Variables.Items;
