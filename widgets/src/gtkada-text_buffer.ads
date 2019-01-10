------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
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

with Glib;
with Gtk.Text_Buffer;

package Gtkada.Text_Buffer is

   type Gtkada_Text_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record  with private;
   type Gtkada_Text_Buffer is access all Gtkada_Text_Buffer_Record;

   procedure Gtk_New (Buffer : out Gtkada_Text_Buffer);
   --  Create a new Text_Buffer_Access.

   procedure Initialize (Buffer : access Gtkada_Text_Buffer_Record'Class);
   --  Internal initialization procedure.
   --  See he section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  The internal type

   procedure Clear (Buffer : access Gtkada_Text_Buffer_Record'Class);
   --  Delete all characters from the given buffer, leaving an empty buffer.

private

   type Gtkada_Text_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with null record;

end Gtkada.Text_Buffer;
