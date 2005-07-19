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
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gtk.Text_Mark;   use Gtk.Text_Mark;

package Gtkada.Text_Buffer is

   type Gtkada_Text_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record  with private;
   type Gtkada_Text_Buffer is access all Gtkada_Text_Buffer_Record;

   procedure Gtk_New (Buffer : out Gtkada_Text_Buffer);
   --  Create a new Text_Buffer_Access.

   procedure Initialize (Buffer : access Gtkada_Text_Buffer_Record'Class);
   --  Internal initialization procedure.
   --  See he section "Creating your own widgets" in the documentation.

   procedure Clear (Buffer : access Gtkada_Text_Buffer_Record'Class);
   --  Delete all characters from the given buffer, leaving an empty buffer.

   procedure Get_Cursor_Position
     (Buffer : access Gtkada_Text_Buffer_Record'Class;
      Iter   : out Gtk_Text_Iter);
   --  Return the current cursor position. Such a query should rather be done
   --  on the specific view in which you are interested

   ---------------
   -- Get / Set --
   ---------------

   function Get_Insert_Mark
     (Buffer : access Gtkada_Text_Buffer_Record'Class)
      return Gtk_Text_Mark;
   pragma Inline (Get_Insert_Mark);
   --  Return the current insert mark.

   procedure Set_Insert_Mark
     (Buffer : access Gtkada_Text_Buffer_Record'Class;
      Mark   : Gtk_Text_Mark);
   pragma Inline (Set_Insert_Mark);
   --  Store the current insert mark.

private

   type Gtkada_Text_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
   with record
      Insert_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  This is a copy of the "insert" mark or the "gtk_drag_target" mark.
      --  This could be easily looked-up when needed, but having a copy is
      --  helping performance-wise, since a  lot of subprograms use it.
      --  This must always be a valid text mark.

   end record;

end Gtkada.Text_Buffer;
