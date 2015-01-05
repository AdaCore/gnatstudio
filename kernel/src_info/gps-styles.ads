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

--  Styles are a GPS-wide resource used to represent color and/or fonts.
--  Styles are used across modules, in all places that require color and
--  fonts that are customizable by the user: Vdiff colors, Search results
--  highlighting, syntax highlighting, Locations view, and so on.

with GNAT.Strings;

package GPS.Styles is

   ------------
   -- Styles --
   ------------
   --  See description for Editor_Overlay

   type Simple_Style_Record is tagged limited record
      Foreground : GNAT.Strings.String_Access;
      Background : GNAT.Strings.String_Access;

      Editor_Icon_Name : GNAT.Strings.String_Access;
      Speedbar   : Boolean := False;
   end record;

   type Simple_Style_Access is access all Simple_Style_Record'Class;
   pragma No_Strict_Aliasing (Simple_Style_Access);

   procedure Set_Foreground
     (Style : not null access Simple_Style_Record; Color : String);
   procedure Set_Background
     (Style : not null access Simple_Style_Record; Color : String);
   --  Set the foreground or background color for Style. Color must be a
   --  recognized color. (Either a simple color, or "#RRGGBB");

   procedure Set_Editor_Icon
     (Style : not null access Simple_Style_Record; Id : String);
   --  Set the icon to use to represent this style in the side of editors

   function Get_Foreground
     (Style : not null access Simple_Style_Record) return String;
   function Get_Background
     (Style : not null access Simple_Style_Record) return String;
   --  Return the background color used for the style
   function Get_Editor_Icon
     (Style : not null access Simple_Style_Record) return String;
   --  Return the editor icon id used for the style

   procedure Set_In_Speedbar
     (Style       : not null access Simple_Style_Record;
      In_Speedbar : Boolean);
   function In_Speedbar
     (Style       : not null access Simple_Style_Record) return Boolean;
   --  Set or get whether a mark should be put in the speedbar when this style
   --  is used within the line

   procedure Free (Style : in out Simple_Style_Record);
   --  Free style.

end GPS.Styles;
