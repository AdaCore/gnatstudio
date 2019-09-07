------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

--  This package provides the settings to interact with the gtk+ icon theme.
--
--  By default, gtk+ loads the Adwaita icon theme, which is found in the
--  directory prefix/share/icons/.
--  An extra theme, hicolor, is also defined, and acts as a fallback when
--  icons are not found in Adwaita. GPS adds extra directories to that
--  hicolor theme (gpsprefix/share/icons/hicolor/*), which contain GPS
--  specific icons. As a result, icons from the user's theme (Adwaita) have
--  priority, and GPS icons are loaded as fallbacks.
--
--  In GPS, the following conventions are used:
--    * All icons should be scalable SVG icons. Currently, a few of these
--      SVG simply embed PNG data, but they should be replaced in the long
--      term, so that the icons display properly on hidpi screens.
--
--    * By convention in gtk+, the basename of the file is the name by
--      which the icon is refered to in the code. gtk+ automatically looks
--      for variants, for instance   basename-rtl.svg when using right-to-left
--      writing conventions.
--
--    * in GPS, icon files most often end with "-symbolic.svg". This is a
--      special convention in gtk+. Such icons are displayed as grayscale
--      only, and automatically adapt to dark themes. This means we do not
--      need to provide multiple versions of the images, depending on the
--      user's theme choice.
--      (For this work, icons must use the fill property, not stroke, since
--      only the former is properly converted)
--      In some cases, however, we want to use colors in the icons. In this
--      case, the icon should not use the -symbolic.svg prefix (although this
--      isn't systematic, since colors are sometimes properly displayed for
--      symbolic icons).
--
--    * Icons must be organized into subdirectories based on their default
--      size. gtk+ 3.14.5 will otherwise display warnings. The size of icons
--      in a directory is given in the index.theme file.
--
--    * We use strings to represent the icons in the GPS code, not string
--      constants. This makes it slightly easier to maintain, and it is
--      assumed that someone adding a new icon will properly test that it
--      displays properly, thus limiting the risk of typos.
--
--    * As much as possible, we keep this icon name until the last moment
--      before rendering. This is cleaner than manipulating and storing
--      Gdk_Pixbuf. For instance, icons will automatically adapt to changes
--      in the icon theme, or to screen resolution.
--
--  Some icons can be found in
--      http://commons.wikimedia.org/wiki/GNOME_Desktop_icons
--  available under GPL2

with Gtk.Enums;
with GNATCOLL.VFS;
with GPS.Kernel;

package GPS.Stock_Icons is

   procedure Register_Stock_Icons
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      System_Dir : GNATCOLL.VFS.Virtual_File);
   --  Register the stock icons for GPS.
   --  System_Dir is the installation prefix for GPS.

   Icon_Size_Action_Button : Gtk.Enums.Gtk_Icon_Size;
   --  A very small icon size (7x7). It is used for instance for the Stop_Task
   --  button in the main toolbar.

   Icon_Size_Local_Toolbar : Gtk.Enums.Gtk_Icon_Size;
   --  The size for local toolbars in the MDI panels.

end GPS.Stock_Icons;
