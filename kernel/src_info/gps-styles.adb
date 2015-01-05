------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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

package body GPS.Styles is

   use GNAT.Strings;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Foreground);
      if Color /= "" then
         Style.Foreground := new String'(Color);
      end if;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Background);
      if Color /= "" then
         Style.Background := new String'(Color);
      end if;
   end Set_Background;

   ---------------------
   -- Set_Editor_Icon --
   ---------------------

   procedure Set_Editor_Icon
     (Style : not null access Simple_Style_Record; Id : String) is
   begin
      Free (Style.Editor_Icon_Name);
      if Id /= "" then
         Style.Editor_Icon_Name := new String'(Id);
      else
         Style.Editor_Icon_Name := null;
      end if;
   end Set_Editor_Icon;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Background = null then
         return "";
      else
         return Style.Background.all;
      end if;
   end Get_Background;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Foreground = null then
         return "";
      else
         return Style.Foreground.all;
      end if;
   end Get_Foreground;

   ---------------------
   -- Get_Editor_Icon --
   ---------------------

   function Get_Editor_Icon
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Editor_Icon_Name = null then
         return "";
      else
         return Style.Editor_Icon_Name.all;
      end if;
   end Get_Editor_Icon;

   ---------------------
   -- Set_In_Speedbar --
   ---------------------

   procedure Set_In_Speedbar
     (Style       : not null access Simple_Style_Record;
      In_Speedbar : Boolean) is
   begin
      Style.Speedbar := In_Speedbar;
   end Set_In_Speedbar;

   -----------------
   -- In_Speedbar --
   -----------------

   function In_Speedbar
     (Style       : not null access Simple_Style_Record) return Boolean is
   begin
      return Style.Speedbar;
   end In_Speedbar;

   ----------
   -- Free --
   ----------

   procedure Free (Style : in out Simple_Style_Record) is
   begin
      Free (Style.Background);
      Free (Style.Foreground);
      Free (Style.Editor_Icon_Name);
   end Free;

end GPS.Styles;
