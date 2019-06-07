------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GNATCOLL.Xref; use GNATCOLL.Xref;

with Gtk.Widget;    use Gtk.Widget;
with GPS.Kernel;    use GPS.Kernel;

package Outline_View is

   Outline_Error : exception;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Outline_Tooltip_Factory_Type is
     access function
       (Kernel      : not null access Kernel_Handle_Record'Class;
        File        : GNATCOLL.VFS.Virtual_File;
        Entity_Name : String;
        Line        : Integer;
        Column      : Visible_Column) return Gtk_Widget;
   --  Type representing a tooltip factory for the Outline view.

   procedure Set_Outline_Tooltip_Factory
     (Tooltip_Factory : not null Outline_Tooltip_Factory_Type);
   --  Set the tooltip factory to use when hovering on the Outline.

   procedure Set_Outline_Tooltips_Synchronous (Synchronous : Boolean);
   --  Set it to True when tooltips created via the factory are ready to be
   --  shown immediately, False otherwise.

end Outline_View;
