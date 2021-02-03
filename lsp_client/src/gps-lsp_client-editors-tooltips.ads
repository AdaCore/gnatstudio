------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2021, AdaCore                  --
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

--  Integration with GNAT Studio's source editor tooltips

with Glib;
with Gtk.Widget;               use Gtk.Widget;
with Pango.Font;

with Basic_Types;              use Basic_Types;
with GPS.Kernel;               use GPS.Kernel;
with Src_Editor_Box;           use Src_Editor_Box;
with Src_Editor_Box.Tooltips;  use Src_Editor_Box.Tooltips;

package GPS.LSP_Client.Editors.Tooltips is

   function Create_LSP_Client_Editor_Tooltip_Handler
     (Box : not null access Source_Editor_Box_Record'Class)
      return Editor_Tooltip_Handler_Access;
   --  LSP-based editor tooltips factory.

   function LSP_Outline_Tooltip_Factory
     (Kernel      : not null access Kernel_Handle_Record'Class;
      File        : GNATCOLL.VFS.Virtual_File;
      Entity_Name : String;
      Line        : Integer;
      Column      : Visible_Column_Type) return Gtk_Widget;
   --  The editor tooltips' factory used when LSP is enabled.

   function Query_Tooltip_For_Entity
     (Kernel              : not null access Kernel_Handle_Record'Class;
      File                : GNATCOLL.VFS.Virtual_File;
      Line                : Integer;
      Column              : Visible_Column_Type;
      For_Global_Tooltips : Boolean := True;
      Xalign              : Glib.Gfloat := 0.0;
      Yalign              : Glib.Gfloat := 0.5;
      Font                : Pango.Font.Pango_Font_Description := null;
      Separator_Expand    : Boolean := False;
      Separator_Padding   : Glib.Guint := 0)
      return Gtk_Widget;
   --  Query a tooltip widget for the given entity, displaying useful
   --  information about it (i.e: declaration and associated comments in most
   --  cases). When For_Glabal_Tooltips is True, this tooltip widget will be
   --  automatically added and displayed in a global tooltip: set this
   --  parameter  to False if you want to display this widget somehwere else.

end GPS.LSP_Client.Editors.Tooltips;
