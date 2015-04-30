------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

--  This package provides the predefined GPS styles

with Language;                 use Language;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Style_Manager; use GPS.Kernel.Style_Manager;

package GPS.Default_Styles is

   procedure Initialize_Default_Styles (Kernel : Kernel_Handle);
   --  Initialize default GPS style definitions

   ------------
   -- Editor --
   ------------

   type Language_Styles_Array is array
     (Standout_Language_Entity) of Style_Access;

   Language_Styles : Language_Styles_Array;
   --  Style corresponding to language elements, used for syntax highlighting

   Editor_Default_Style : Style_Access;
   --  The style for normal text in the editor

   -----------
   -- Build --
   -----------

   type Builder_Message_Category is (Errors, Warnings, Style, Info);
   type Builder_Message_Styles is
     array (Builder_Message_Category) of Style_Access;

   Builder_Styles : Builder_Message_Styles;

   Builder_Background_Style : Style_Access;
   Builder_Shadow_Style     : Style_Access;

   ------------
   -- Search --
   ------------

   Search_Results_Style     : Style_Access;
   --  Style used to highlight search results

end GPS.Default_Styles;
