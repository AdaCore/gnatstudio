------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

--  This package provides support for displaying tooltips in the editors.
--  These tooltips can be overridden by any module in GPS, but they also have
--  a default value which is extracted from the cross-references information.

with Tooltips;
with Gtk.Widget;
with GPS.Kernel; use GPS.Kernel;

package Src_Editor_Box.Tooltips is

   type Editor_Tooltip_Handler is
     new Standard.Tooltips.Tooltip_Handler with private;
   type Editor_Tooltip_Handler_Access is
     access all Editor_Tooltip_Handler'Class;

   overriding function Create_Contents
     (Tooltip  : not null access Editor_Tooltip_Handler;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the overall editor tooltip widget.
   --
   --  Default editor tooltips contain three parts:
   --    - Module specific information (CodePeer, GVD...). if any
   --    - The messages associated to the hovered lin, if any
   --    - Information about the hovered entity, if any
   --
   --  In general you won't need to override this function: overriding
   --  the Get_Tooltip_Widget_For_Entity or the GPS.Kernel.Compute_Tooltip
   --  functions should be enough.

   function Get_Tooltip_Widget_For_Entity
     (Tooltip : not null access Editor_Tooltip_Handler;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget;
   --  Return the widget that will be displayed in editor tooltips when
   --  hovering on an entity.
   --
   --  This function is called only if the user is hovering on an entity: it's
   --  not called when the user if hovering on the editor's side area for
   --  instance.
   --
   --  This default implementation is based on the default cross-references
   --  engine: override it if you want to display non-xref based information
   --  of if you ware using another mechanism to retrieve information about
   --  the given context entity.

   procedure Set_Source_Editor_Box
     (Tooltip : not null access Editor_Tooltip_Handler;
      Box     : not null access Source_Editor_Box_Record'Class);
   --  Associate the given source editor box to the given editor tooltip
   --  handler.

   function Get_Source_Editor_Box
     (Tooltip : not null access Editor_Tooltip_Handler)
      return Source_Editor_Box;
   --  Get the source editor box associated to the editor tooltip handler.

   type Editor_Tooltip_Handler_Factory_Access is
     access function
       (Box : not null access Source_Editor_Box_Record'Class)
        return Editor_Tooltip_Handler_Access;
   --  Type representing an editor tooltip handler factory.
   --  Don't forget to associate the given source editor box to your tooltip
   --  handler when creating new factories.

   function Default_Editor_Tooltip_Handler_Factory
     (Box : not null access Source_Editor_Box_Record'Class)
      return Editor_Tooltip_Handler_Access;
   --  The default editor tooltip handler factory.

private

   type Editor_Tooltip_Handler is new Standard.Tooltips.Tooltip_Handler with
   record
      Box : Source_Editor_Box;
   end record;

end Src_Editor_Box.Tooltips;
