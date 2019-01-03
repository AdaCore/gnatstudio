------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

--  This package handles the commands necessary to configure/add/remove
--  line highlighting in the source editors.

with Gdk.RGBA; use Gdk.RGBA;
with GNATCOLL.Scripts;

package Src_Editor_Module.Line_Highlighting is

   procedure Edit_Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the source editor module.

   --  The following functions are used to provide a cache-like capability
   --  for storing and retrieving categories.
   --  The category indexes begin at 1.

   procedure Add_Category (Style : Style_Access);
   --  Add a new category to the category cache.
   --  Update the Color of the category if it already exists.

   function Lookup_Category (Style : Style_Access) return Natural;
   --  Return the index corresponding to Style.
   --  If there is no category corresponding to Style, create one.

   function Get_Color (Index : Natural) return Gdk_RGBA;
   --  Return the Color corresponding to a category Index.
   --  If Index does not correspond to an existing category, return null.

   function Get_Name (Index : Natural) return String;
   --  Return the name of the highlighting at index. Used for debug purposes.

   function Get_Last_Index return Natural;
   --  Return the number of categories.

end Src_Editor_Module.Line_Highlighting;
