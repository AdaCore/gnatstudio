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

with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Icon_Factory;  use Gtk.Icon_Factory;
with Gtk.Icon_Set;      use Gtk.Icon_Set;
with Gtk.Icon_Source;   use Gtk.Icon_Source;
with Gtk.Settings;      use Gtk.Settings;
with Gtk.Stock;         use Gtk.Stock;
with GPS.Kernel.MDI;    use GPS.Kernel, GPS.Kernel.MDI;

package body GPS.Stock_Icons is
   Me : constant Trace_Handle := Create ("STOCK");

   --------------------------
   -- Register_Stock_Icons --
   --------------------------

   procedure Register_Stock_Icons
     (Kernel     : not null access Kernel_Handle_Record'Class;
      System_Dir : GNATCOLL.VFS.Virtual_File)
   is
      W, H    : Gint;
      Result  : Boolean;
      Set     : Gtk_Icon_Set;
      pragma Unreferenced (System_Dir, Set);
   begin
      Icon_Size_Action_Button := Icon_Size_Register ("ICON_SIZE_ACTION", 7, 7);
      Icon_Size_Local_Toolbar :=
         Icon_Size_Register ("ICON_SIZE_LOCAL_TOOLBAR", 12, 12);
      Icon_Size_Speedbar := Icon_Size_Register ("ICON_SIZE_SPEEDBAR", 9, 9);

      if Active (Me) then
         Icon_Size_Lookup (Icon_Size_Action_Button, W, H, Result);
         Trace (Me, "Icon size Action =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Local_Toolbar, W, H, Result);
         Trace (Me, "Icon size Local Toolbar =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Menu, W, H, Result);
         Trace (Me, "Icon size Menu =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Small_Toolbar, W, H, Result);
         Trace (Me, "Icon size Small_Toolbar =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Large_Toolbar, W, H, Result);
         Trace (Me, "Icon size Large_Toolbar =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Button, W, H, Result);
         Trace (Me, "Icon size Button =>" & W'Img & "x" & H'Img);
      end if;

      --  Register an initial value for some of the icons. For instance, the
      --  Messages view has a local config menu and is created before we load
      --  icons.py. We need however to have the stock icon registered or the
      --  button will not be updated when we actually load the icons.
      --  The exact path is irrelevant here.

      Set := Set_Icon
        (Kernel, GPS_Stock_Config_Menu, "", Create (+"svg/menu.svg"));
   end Register_Stock_Icons;

   ---------------------------
   -- Icon_To_Absolute_Path --
   ---------------------------

   procedure Icon_To_Absolute_Path
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Filename : in out GNATCOLL.VFS.Virtual_File)
   is
      Dark_Theme : Boolean;
      Base, Tmp : Virtual_File;
   begin
      if not Filename.Is_Absolute_Path then
         Dark_Theme := Glib.Properties.Get_Property
           (Gtk.Settings.Get_Default,
            Gtk.Settings.Gtk_Application_Prefer_Dark_Theme_Property);

         if not Dark_Theme then
            Base := Create_From_Dir (Kernel.Get_Share_Dir, "icons/dark");
            Filename := Create_From_Dir (Base, Filename.Full_Name);
         else
            --  Load icons from light icon theme, but if not found use the
            --  default one from the dark theme.
            Base := Create_From_Dir (Kernel.Get_Share_Dir, "icons/light");
            Tmp := Create_From_Dir (Base, Filename.Full_Name);

            if Tmp.Is_Regular_File then
               Filename := Tmp;
            else
               Base := Create_From_Dir (Kernel.Get_Share_Dir, "icons/dark");
               Filename := Create_From_Dir (Base, Filename.Full_Name);
            end if;
         end if;
      end if;

      if not Filename.Is_Regular_File then
         Trace (Me, "File not found " & Filename.Display_Full_Name);
         Filename := No_File;
      end if;
   end Icon_To_Absolute_Path;

   --------------
   -- Set_Icon --
   --------------

   function Set_Icon
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Id     : String;
      Label  : String;
      File   : GNATCOLL.VFS.Virtual_File) return Gtk.Icon_Set.Gtk_Icon_Set
   is
      Factory : constant Gtk_Icon_Factory := Get_Icon_Factory (Kernel);
      Source   : Gtk_Icon_Source;
      Set      : Gtk_Icon_Set;
      Pic_File : GNATCOLL.VFS.Virtual_File := File;
      Stock    : Gtk_Stock_Item;
   begin
      Icon_To_Absolute_Path (Kernel, Pic_File);

      if Pic_File /= No_File then
         Gtk_New (Set);

         Gtk_New (Source);
         Source.Set_Filename (+Pic_File.Full_Name (True));
         Set.Add_Source (Source);
         Free (Source);

         Factory.Add (Id, Set);
         Set.Unref;

         Gtk_New (Stock, Id, Label, 0, 0, "");
         Add (Stock);
         Free (Stock);  --  gtk+ took its own copy

         return Set;
      else
         return Null_Gtk_Icon_Set;
      end if;
   end Set_Icon;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Set    : Gtk.Icon_Set.Gtk_Icon_Set;
      File   : GNATCOLL.VFS.Virtual_File;
      Size   : Gtk.Enums.Gtk_Icon_Size)
   is
      Source   : Gtk_Icon_Source;
      Pic_File : GNATCOLL.VFS.Virtual_File := File;
   begin
      Icon_To_Absolute_Path (Kernel, Pic_File);

      if Pic_File /= No_File then
         Gtk_New (Source);
         Source.Set_Filename (+Pic_File.Full_Name (True));
         Source.Set_Size (Size);
         Source.Set_Size_Wildcarded (False);
         Set.Add_Source (Source);
         Free (Source);
      end if;
   end Set_Icon;

end GPS.Stock_Icons;
