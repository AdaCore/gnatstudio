-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004                         --
--                            ACT-Europe                             --
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

with Glib.Object;           use Glib.Object;
with Glib.Xml_Int;          use Glib.Xml_Int;
with Glide_Kernel;          use Glide_Kernel;
with Gtk.Menu;              use Gtk.Menu;
with Case_Handling;         use Case_Handling;

package Casing_Exceptions is

   procedure Add_Exception (Ident : String);
   --  Add Ident into the case exception table

   procedure Remove_Exception (Ident : String);
   --  Remove Ident from the case exception table

   function Get_Case_Exceptions return Case_Handling.Casing_Exceptions;
   --  Return the current case exception table

   ---------------------------
   -- Casing Initialisation --
   ---------------------------

   procedure Casing_Contextual
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Build the casing contextual memu

   procedure Casing_Initialize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize the casing support, must be called before other calls, it
   --  reads the user's casing_exceptions.xml files.

   procedure Casing_Finalize
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Finalize the casing support, it saves the casing exception table
   --  to the user's casing_exceptions.xml file.

   procedure Casing_Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Customization routine for the casing feature, this is a callback to
   --  be used with a Register_Module.

end Casing_Exceptions;
