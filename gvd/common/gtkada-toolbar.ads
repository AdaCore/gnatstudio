-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Box;
with Gtk.Toolbar;
with Gtk.Handle_Box;
with Gtk.Toolbar;
with Gtk.Enums; use Gtk.Enums;

package Gtkada.Toolbar is

   type Gtkada_Toolbar_Record is new
     Gtk.Toolbar.Gtk_Toolbar_Record with private;
   --  Gtk.Handle_Box.Gtk_Handle_Box_Record with private;
   type Gtkada_Toolbar is access all Gtkada_Toolbar_Record'Class;

   procedure Gtk_New
     (Toolbar     : out Gtkada_Toolbar;
      Orientation : in Gtk_Orientation;
      Style       : in Gtk_Toolbar_Style);
   --  Note: this procedure is provided for compatibility with Gtk_Toolbar.
   --  The Orientation parameter is actually ignored.

   procedure Gtk_New
     (Toolbar     : out Gtkada_Toolbar;
      Style       : in Gtk_Toolbar_Style);

   procedure Initialize
     (Toolbar : access Gtkada_Toolbar_Record'Class;
      Style   : in Gtk_Toolbar_Style);

   function Get_Handle_Box
     (Toolbar : access Gtkada_Toolbar_Record)
      return Gtk.Handle_Box.Gtk_Handle_Box;

private
   --  type Gtkada_Toolbar_Record is new Gtk.Handle_Box.Gtk_Handle_Box_Record with
   type Gtkada_Toolbar_Record is new Gtk.Toolbar.Gtk_Toolbar_Record with
   record
      Box           : Gtk.Box.Gtk_Hbox;
      Toolbar_Right : Gtk.Toolbar.Gtk_Toolbar;
      Handle_Box    : Gtk.Handle_Box.Gtk_Handle_Box;
      Style         : Gtk_Toolbar_Style;
   end record;

end Gtkada.Toolbar;
