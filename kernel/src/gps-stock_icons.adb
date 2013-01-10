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

with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Glib.Error;        use Glib, Glib.Error;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Icon_Factory;  use Gtk.Icon_Factory;
with Gtk.Icon_Set;      use Gtk.Icon_Set;
with Gtk.Icon_Source;   use Gtk.Icon_Source;
with GPS.Intl;          use GPS.Intl;
with Gtk.Stock;         use Gtk.Stock;

package body GPS.Stock_Icons is
   Me : constant Trace_Handle := Create ("STOCK");

   --------------------------
   -- Register_Stock_Icons --
   --------------------------

   procedure Register_Stock_Icons (System_Dir : GNATCOLL.VFS.Virtual_File) is
      Sys : constant Virtual_File :=
        System_Dir.Create_From_Dir ("share/gps/icons");
      W, H    : Gint;
      Result  : Boolean;
      Factory : Gtk_Icon_Factory;

      procedure Icon (Stock, Label : String; Path : Filesystem_String;
                      Wildcarded : Boolean := True;
                      Size : Gtk_Icon_Size := Icon_Size_Small_Toolbar);
      --  Register a single icon.
      --  If Wildcarded is false, then the icon cannot be used at other sizes,
      --  although it is set to be best for Size.

      procedure Icon (Stock, Label : String; Path : Filesystem_String;
                      Wildcarded : Boolean := True;
                      Size : Gtk_Icon_Size := Icon_Size_Small_Toolbar)
      is
         P : constant Virtual_File := Sys.Create_From_Dir (Path);
         Set     : Gtk_Icon_Set;
         Source  : Gtk_Icon_Source;
         Pixbuf  : Gdk_Pixbuf;
         Error   : GError;
         Item    : Gtk_Stock_Item;
      begin
         if Path /= "" then
            if not Wildcarded then
               Gtk_New (Source);
               Source.Set_Size_Wildcarded (False);
               Source.Set_Size (Size);
               Source.Set_Filename (P.Display_Full_Name);
               Source.Set_Icon_Name (Stock);

               Gtk_New (Set);
               Set.Add_Source (Source);

            else
               Gdk_New_From_File (Pixbuf, +P.Full_Name.all, Error);

               if Error /= null then
                  Trace (Me, "Error loading " & P.Display_Full_Name & ": "
                         & Get_Message (Error));
               else
                  Gtk_New_From_Pixbuf (Set, Pixbuf);
               end if;
            end if;

            Factory.Add (Stock, Set);
         end if;

         if Label /= "" then
            Gtk_New (Item, Stock_Id => Stock, Label => Label,
                     Modifier => 0, Keyval => 0, Translation_Domain => "");
            Add (Item);
         end if;
      end Icon;

   begin
      Icon_Size_Action_Button := Icon_Size_Register ("ICON_SIZE_ACTION", 7, 7);
      Icon_Size_Local_Toolbar :=
         Icon_Size_Register ("ICON_SIZE_LOCAL_TOOLBAR", 12, 12);

      if Active (Me) then
         Trace (Me, "Icons dir=" & Sys.Display_Full_Name);

         Icon_Size_Lookup (Icon_Size_Action_Button, W, H, Result);
         Trace (Me, "Icon size Action =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Menu, W, H, Result);
         Trace (Me, "Icon size Menu =>" & W'Img & "x" & H'Img);

         Icon_Size_Lookup (Icon_Size_Small_Toolbar, W, H, Result);
         Trace (Me, "Icon size Small_Toolbar =>" & W'Img & "x" & H'Img);
      end if;

      Gtk_New (Factory);
      Add_Default (Factory);

      Icon (GPS_Stock_Config_Menu, -"Configuration", "20px/menu_20.png");
      Icon (GPS_Stop_Task, -"Close", "9px/close_8.png");
      Icon (GPS_Logo, -"Logo", "24px/gps_24.png");
      Icon (GPS_Expand_All, -"Expand all", "9px/expand_12.png");
      Icon (GPS_Collapse_All, "-Collapse all", "9px/collapse_12.png");
      Icon (GPS_Clear_Entry, -"Clear", "16px/clear_entry_16.png");
      Icon (GPS_Toggle_Links, -"Hide/Show links", "9px/toggle_links_12.png");
      Icon (GPS_Remove_Unselected, -"Remove unselected",
            "9px/remove_unselected_12.png");
      Icon (GPS_Read_Only, -"Read only", "16px/lock_16.png");
      Icon (GPS_Writable, -"Writable", "16px/unlock_16.png");
      Icon (GPS_Regexp, -"Regexp", "16px/regexp.png");
      Icon (GPS_Negate_Search, -"Invert search", "16px/negate.png");
   end Register_Stock_Icons;

end GPS.Stock_Icons;
