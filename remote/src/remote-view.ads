-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2009, AdaCore                  --
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

--  This package provides a view that allows the remote servers configuration

with Glib.Object;
with Gtk.Widget;
with Gtkada.MDI;

with GPS.Kernel;
with GPS.Kernel.Modules;
with XML_Utils;

package Remote.View is

   function Load_Desktop
     (Module : GPS.Kernel.Modules.Module_ID;
      Node   : XML_Utils.Node_Ptr;
      User   : GPS.Kernel.Kernel_Handle) return Gtkada.MDI.MDI_Child;
   function Save_Desktop
     (Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      User        : GPS.Kernel.Kernel_Handle;
      Module_Name : String) return XML_Utils.Node_Ptr;
   --  Load and save desktop

   procedure Show_Remote_View
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID);
   --  Show the remote view MDI child

end Remote.View;
