-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005-2006                    --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides XML Tree viewers

with Glib.Xml_Int;
with GPS.Kernel; use GPS.Kernel;
with Gtk.Box;
with Gtk.Tree_Model;

package XML_Viewer is

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the shell commands associated with this module

private
   type Abstract_XML_Viewer_Record is abstract new Gtk.Box.Gtk_Vbox_Record with
      null record;

   function Node_Parser
     (View        : access Abstract_XML_Viewer_Record;
      Parent      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node        : Glib.Xml_Int.Node_Ptr;
      Child_Index : Positive) return Gtk.Tree_Model.Gtk_Tree_Iter is abstract;
   --  Parse Node, and create a new row in View.
   --  Returns an iterator to the newly created row. If Null_Iter is returned,
   --  then we will not iterate over children of Node, which can be used for
   --  instance if you have already used these children to create the new row.
   --
   --  Child_Index is set to 1 for the first XML child of a given node, and
   --  increased for each children.

   function On_Click
     (View         : access Abstract_XML_Viewer_Record;
      Double_Click : Boolean;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node         : Glib.Xml_Int.Node_Ptr) return Boolean is abstract;
   --  Called when a row of the table is clicked on.
   --  Return true if an action was performed

   procedure Free (View : access Abstract_XML_Viewer_Record) is abstract;
   --  Free the memory occupied by View

end XML_Viewer;
