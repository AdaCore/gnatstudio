------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package provides stock icons to use in the context of GPS.
--  GPS should avoid using hard-coded path names for icons, so that it is
--  easier to replace them everywhere, and gtk+ can automatically resize
--  images as appropriate. It is possible for GPS to register the same images
--  in various sizes to get the best rendering possible.
--
--  See also the file share/gps/plug-ins/icons.xml
--  which can be used to define standard icons.
--  However, the corresponding icons do not have a corresponding string
--  constant.

with Gtk.Enums;
with GNATCOLL.VFS;

package GPS.Stock_Icons is

   procedure Register_Stock_Icons (System_Dir : GNATCOLL.VFS.Virtual_File);
   --  Register the stock icons for GPS.
   --  System_Dir is the installation prefix for GPS.

   GPS_Stock_Config_Menu : constant String := "gps-configMenu";
   --  Icon used for the configuration menu in the toolbar of views

   GPS_Stop_Task : constant String := "gps-stopTask";
   --  Icon used in the task manager's main progress bar to cancel a task

   GPS_Logo : constant String := "gps-logo";
   --  The GPS logo

   GPS_Expand_All : constant String := "gps-expandAll";
   GPS_Collapse_All : constant String := "gps-collapseAll";
   --  Manipulating a tree view

   GPS_Clear_Entry : constant String := "gps-clear-entry";
   --  Clear the contents of the entry, to be used with
   --  Gtk.GEntry.Set_Icon_From_Stock.

   Icon_Size_Action_Button : Gtk.Enums.Gtk_Icon_Size;
   --  A very small icon size (7x7). It is used for instance for the Stop_Task
   --  button in the main toolbar.

   Icon_Size_Local_Toolbar : Gtk.Enums.Gtk_Icon_Size;
   --  The size for local toolbars in the MDI panels.

end GPS.Stock_Icons;
