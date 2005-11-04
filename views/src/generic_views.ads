-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
--                              AdaCore                              --
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

--  This package helps build simple views that are associated with a single
--  window, that are saved in the desktop, and have a simple menu in Tools/
--  to open them.
--  This package must be instanciated at library-level

with GPS.Kernel.Modules;
with Glib.Object;
with Glib.Xml_Int;
with Gtk.Widget;
with Gtkada.MDI;

generic
   Module_Name : String;
   --  The name of the module, and name used in the desktop file. It mustn'y
   --  contain any space

   View_Name   : String;
   --  Name of the menu, in tools, that is used to create the view. It is also
   --  used as the name for the MDI window.

   type View_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   --  Type of the widget representing the view

   with procedure Initialize
     (View   : access View_Record'Class;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is <>;
   --  Function used to create the view itself

package Generic_Views is

   type View_Access is access all View_Record'Class;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      ID     : GPS.Kernel.Modules.Module_ID := null);
   --  Register the module. This sets it up for proper desktop handling, as
   --  well as create a menu in Tools/ so that the user can open the view.
   --  ID can be passed in parameter if a special tagged type needs to be
   --  used.

   function Get_Module return GPS.Kernel.Modules.Module_ID;
   --  Return the module ID corresponding to that view

   function Get_Or_Create_View
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Reuse_If_Exist : Boolean := True)
      return View_Access;
   --  Return the view (create a new one if necessary, or always if
   --  Reuse_If_Exist is False).
   --  The view gets the focus automatically

private
   --  The following subprograms need to be in the spec so that we can get
   --  access to them from callbacks in the body

   procedure On_Open_View
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Create a new view if none exists, or raise the existing one

   function Load_Desktop
     (MDI  : Gtkada.MDI.MDI_Window;
      Node : Glib.Xml_Int.Node_Ptr;
      User : GPS.Kernel.Kernel_Handle) return Gtkada.MDI.MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : GPS.Kernel.Kernel_Handle)
      return Glib.Xml_Int.Node_Ptr;
   --  Support functions for the MDI

end Generic_Views;
