------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2020, AdaCore                     --
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

with Glib;                   use Glib;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Icon_Factory;       use Gtk.Icon_Factory;
with Gtk.Icon_Theme;         use Gtk.Icon_Theme;

with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Custom;      use GPS.Kernel.Custom;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;

package body GPS.Stock_Icons is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.STOCK");

   --------------------------
   -- Register_Stock_Icons --
   --------------------------

   procedure Register_Stock_Icons
     (Kernel     : not null access Kernel_Handle_Record'Class;
      System_Dir : Virtual_File)
   is
      W, H     : Gint;
      Result   : Boolean;
      Theme    : Gtk_Icon_Theme;
      GNATStudio_Home_Dir : constant Virtual_File := Kernel.Get_Home_Dir;
   begin
      Icon_Size_Local_Toolbar :=
        Icon_Size_Register ("ICON_SIZE_LOCAL_TOOLBAR", 12, 12);
      Icon_Size_Local_Toolbar_Large :=
        Icon_Size_Register ("ICON_SIZE_LOCAL_TOOLBAR_LARGE", 16, 16);

      if Active (Me) then
         Icon_Size_Lookup (Icon_Size_Local_Toolbar, W, H, Result);
         Trace (Me, "Icon size Local Toolbar =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Local_Toolbar_Large, W, H, Result);
         Trace (Me, "Icon size Local Toolbar Large =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Menu, W, H, Result);
         Trace (Me, "Icon size Menu =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Small_Toolbar, W, H, Result);
         Trace (Me, "Icon size Small_Toolbar =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Large_Toolbar, W, H, Result);
         Trace (Me, "Icon size Large_Toolbar =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Button, W, H, Result);
         Trace (Me, "Icon size Button =>" & W'Img & "x" & H'Img);
      end if;

      --  Setup an icon theme, pointing to the local directory where
      --  GNAT Studio has its icons.
      --  This directory should contain one or more subdirectories, whose name
      --  match the icon themes. We use 'hicolor', since that's the fallback
      --  when the user's theme (Adwaita) does not contain an icon.
      --  Subdirectories must match those defined by the theme
      --  i.e. in the file   prefix/share/icons/hicolor/index.theme.

      Theme := Get_Default;
      Theme.Prepend_Search_Path
        (Create_From_Dir
           (Create_From_Dir
              (Create_From_Dir (System_Dir, "share"),
               "gnatstudio"),
            "icons")
         .Display_Full_Name);

      Theme.Prepend_Search_Path
        (Create_From_Dir (GNATStudio_Home_Dir, "icons").Display_Full_Name);

      --  Add each directory from the custom path
      declare
         P : constant File_Array := Get_Custom_Path;
      begin
         for F of P loop
            Theme.Prepend_Search_Path (F.Display_Full_Name);
         end loop;
      end;
   end Register_Stock_Icons;

   --------------------------------------
   -- Get_Icon_Size_For_Local_Toolbars --
   --------------------------------------

   function Get_Icon_Size_For_Local_Toolbars return Gtk.Enums.Gtk_Icon_Size
   is
     (if Toolbar_Icons_Size'(Pref_Toolbar_Style.Get_Pref) = Large_Icons then
         Icon_Size_Local_Toolbar_Large
      else
         Icon_Size_Local_Toolbar);

end GPS.Stock_Icons;
