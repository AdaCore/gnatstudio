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
with Glib;              use Glib;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Icon_Factory;  use Gtk.Icon_Factory;

package body GPS.Stock_Icons is
   Me : constant Trace_Handle := Create ("STOCK");

   --------------------------
   -- Register_Stock_Icons --
   --------------------------

   procedure Register_Stock_Icons (System_Dir : GNATCOLL.VFS.Virtual_File) is
      pragma Unreferenced (System_Dir);
      W, H    : Gint;
      Result  : Boolean;
   begin
      Icon_Size_Action_Button := Icon_Size_Register ("ICON_SIZE_ACTION", 7, 7);
      Icon_Size_Local_Toolbar :=
         Icon_Size_Register ("ICON_SIZE_LOCAL_TOOLBAR", 12, 12);

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
   end Register_Stock_Icons;

end GPS.Stock_Icons;
