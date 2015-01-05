------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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
--
--  In general, the stock_id can be used as is when creating widgets like
--  buttons, since gtk+ supports them out of the box. When you want to use them
--  in browsers, things are more complex (in fact, it seems that the svg are
--  displayed at a low resolution and then scaled, losing the advantage of
--  vector drawing...). Two solutions that have been used in the past:
--
--      P : Gdk_Pixbuf;
--      Error : GError;
--      Path : Virtual_File := Create ("svg/refresh.svg");
--      Icon_To_Absolute_Path (Kernel, Path);
--      Gdk_New_From_File (P, Path.Display_Full_Name, Error);
--
--  or
--
--      P := Widget.Render_Icon_Pixbuf
--         (Stock_Id => ..., Size => Icon_Size_Button);

with Gtk.Enums;
with Gtk.Icon_Set;
with GNATCOLL.VFS;
with GPS.Kernel;

package GPS.Stock_Icons is

   procedure Register_Stock_Icons
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      System_Dir : GNATCOLL.VFS.Virtual_File);
   --  Register the stock icons for GPS.
   --  System_Dir is the installation prefix for GPS.

   GPS_Stock_Config_Menu : constant String := "gps-configMenu";
   --  Icon used for the configuration menu in the toolbar of views

   GPS_Stop_Task : constant String := "gps-stopTask";
   --  Icon used in the task manager's main progress bar to cancel a task

   GPS_Expand_All : constant String := "gps-expandAll";
   GPS_Collapse_All : constant String := "gps-collapseAll";
   --  Manipulating a tree view

   GPS_Clear_Entry : constant String := "gps-clear-entry";
   --  Clear the contents of the entry, to be used with
   --  Gtk.GEntry.Set_Icon_From_Stock.

   GPS_Toggle_Links : constant String := "gps-toggle-links";
   --  Hide or Show links in a browser

   GPS_Remove_Unselected : constant String := "gps-remove-unselected";
   --  Remove unselected elements

   GPS_Read_Only : constant String := "gps-read-only";
   GPS_Writable  : constant String := "gps-writable";
   --  Locl and unlock icons

   GPS_Fold_Block : constant String := "gps-fold-block";
   GPS_Unfold_Block : constant String := "gps-unfold-block";
   --  Source editor speedbar icons

   GPS_Regexp : constant String := "gps-regexp";
   GPS_Negate_Search : constant String := "gps-negate";

   GPS_Double_Arrow : constant String := "gps-double-arrow";
   --  Two arrows aligned vertically, to show that there is a combo box

   Icon_Size_Action_Button : Gtk.Enums.Gtk_Icon_Size;
   --  A very small icon size (7x7). It is used for instance for the Stop_Task
   --  button in the main toolbar.

   Icon_Size_Local_Toolbar : Gtk.Enums.Gtk_Icon_Size;
   --  The size for local toolbars in the MDI panels.

   Icon_Size_Speedbar : Gtk.Enums.Gtk_Icon_Size;

   GPS_Refresh             : constant String := "gps-refresh";
   --  Refresh views

   GPS_Edit_Value          : constant String := "gps-edit";
   --  Edit a value in a view

   GPS_Save                : constant String := "gtk-cdrom";
   --  The icon for save. We avoid using "gtk-save" because of a startup
   --  interaction with gio.

   function Set_Icon
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : String;
      Label  : String;
      File   : GNATCOLL.VFS.Virtual_File) return Gtk.Icon_Set.Gtk_Icon_Set;
   procedure Set_Icon
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Set    : Gtk.Icon_Set.Gtk_Icon_Set;
      File   : GNATCOLL.VFS.Virtual_File;
      Size   : Gtk.Enums.Gtk_Icon_Size);
   --  Register a new stock icon.
   --  File is either an absolute file name, or relative to the icons directory
   --  in the GPS install.
   --  The second version can be used to add variants for specific sizes.

   procedure Icon_To_Absolute_Path
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : in out GNATCOLL.VFS.Virtual_File);
   --  Return the proper location for an icon
   --  Sets Filename to No_File if the file is not found.

end GPS.Stock_Icons;
