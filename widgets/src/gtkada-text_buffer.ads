-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                   Copyright (C) 2005 AdaCore                      --
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

with Gtk.Text_Buffer;

package Gtkada.Text_Buffer is

   type Text_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record  with private;
   type Text_Buffer is access all Text_Buffer_Record;

   procedure Gtk_New (Buffer : out Text_Buffer);
   --  Create a new Text_Buffer.

   procedure Initialize (Buffer : access Text_Buffer_Record'Class);
   --  Internal initialization procedure.
   --  See he section "Creating your own widgets" in the documentation.

   procedure Clear (Buffer : access Text_Buffer_Record'Class);
   --  Delete all characters from the given buffer, leaving an empty buffer.

private

   type Text_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with null record;

end Gtkada.Text_Buffer;
