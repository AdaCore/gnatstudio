------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
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

with Gtk.Text_View;
with Gtkada.Text_Buffer; use Gtkada.Text_Buffer;

package Gtkada.Text_View is

   type Gtkada_Text_View_Record is
     new Gtk.Text_View.Gtk_Text_View_Record  with private;
   type Gtkada_Text_View is access all Gtkada_Text_View_Record'Class;

   procedure Gtk_New
     (View   : out Gtkada_Text_View;
      Buffer : Gtkada_Text_Buffer := null);
   --  Create a new Text_View_Access.

   procedure Initialize
     (View   : access Gtkada_Text_View_Record'Class;
      Buffer : Gtkada_Text_Buffer);
   --  Internal initialization procedure.
   --  See he section "Creating your own widgets" in the documentation.

private

   type Gtkada_Text_View_Record is
     new Gtk.Text_View.Gtk_Text_View_Record with null record;

end Gtkada.Text_View;
