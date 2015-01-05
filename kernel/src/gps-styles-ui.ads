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

with Gdk.RGBA;   use Gdk.RGBA;
with Gdk.Pixbuf; use Gdk.Pixbuf;

package GPS.Styles.UI is

   type Style_Record is new Simple_Style_Record with record
      Name        : GNAT.Strings.String_Access;
      Description : GNAT.Strings.String_Access;
      --  A short description of the style

      Editor_Icon : Gdk_Pixbuf := Null_Pixbuf;

      Fg_Color   : Gdk_RGBA := Null_RGBA;
      Bg_Color   : Gdk_RGBA := Null_RGBA;
   end record;

   type Style_Access is access all Style_Record'Class;
   pragma No_Strict_Aliasing (Style_Access);

   ---------------------------
   -- GPS predefined styles --
   ---------------------------

   --  These are the styles defined by default in GPS.

   type Builder_Message_Category is (Errors, Warnings, Style, Info);

   type Builder_Message_Styles is
     array (Builder_Message_Category) of Style_Access;

   Builder_Styles : Builder_Message_Styles;

   Builder_Background_Style : Style_Access;
   Builder_Shadow_Style     : Style_Access;
   Search_Results_Style     : Style_Access;

   overriding procedure Set_Foreground
     (Style : not null access Style_Record; Color : String);
   overriding procedure Set_Background
     (Style : not null access Style_Record; Color : String);

   --  Note: when adding default styles, do not forget to update
   --  Initialize_Predefined_Styles and Preferences_Changed.

   function Get_Background_Color
     (Style : not null access Style_Record) return Gdk_RGBA;
   --  Return the background GC stored in Style. Return Null_GC if there is
   --  none.

   function Get_Editor_Icon
     (Style  : not null access Style_Record) return Gdk_Pixbuf;
   --  Return the default icon to use when displaying this style in the
   --  side of editors.

   function Get_Name (Style : Style_Access) return String;
   --  Return the name of Style.

   procedure Free (Style : in out Style_Access);
   --  Free memory occupied by Style.

private

   overriding procedure Free (Style : in out Style_Record);

end GPS.Styles.UI;
