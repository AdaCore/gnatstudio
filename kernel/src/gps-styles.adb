-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2010, AdaCore                      --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package body GPS.Styles is

   use GNAT.Strings;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Foreground);
      Style.Foreground := new String'(Color);
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Background);
      Style.Background := new String'(Color);
   end Set_Background;

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
   end Free;

end GPS.Styles;
