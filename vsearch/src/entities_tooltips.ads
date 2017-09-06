------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

--  This package provides functions for drawing tooltips representing entity
--  informations.

with Gtk.Widget;

with GPS.Kernel;                      use GPS.Kernel;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Xref;

package Entities_Tooltips is

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Xref.Root_Entity'Class;
      Ref           : Xref.Root_Entity_Reference'Class;
      Draw_Border   : Boolean) return Gtk.Widget.Gtk_Widget;
   --  Return a tooltip representing Entity.

   function Draw_Tooltip
     (Kernel      : access Kernel_Handle_Record'Class;
      Entity      : Semantic_Node'Class;
      Draw_Border : Boolean;
      Guess       : Boolean := False)
      return Gtk.Widget.Gtk_Widget;
   --  Same as above, based on an entity access. If guess is true then the
   --  entity information is a guess - may not be the actual one for the
   --  tooltip.

end Entities_Tooltips;
