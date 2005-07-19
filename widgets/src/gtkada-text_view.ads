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

with Gtk.Text_Iter;
with Gtk.Text_Mark;      use Gtk.Text_Mark;
with Gtk.Text_View;
with Gtkada.Text_Buffer; use Gtkada.Text_Buffer;

package Gtkada.Text_View is

   type Text_View_Record is
     new Gtk.Text_View.Gtk_Text_View_Record  with private;
   type Text_View_Access is access all Text_View_Record'Class;

   procedure Gtk_New
     (View   : out Text_View_Access;
      Buffer : Text_Buffer_Access := null);
   --  Create a new Text_View_Access.

   procedure Initialize
     (View   : access Text_View_Record'Class;
      Buffer : Text_Buffer_Access);
   --  Internal initialization procedure.
   --  See he section "Creating your own widgets" in the documentation.

   procedure Save_Cursor_Position
     (View : access Text_View_Record'Class);
   --  Save the cursor position

   procedure Get_Cursor_Position
     (View : access Text_View_Record'Class;
      Iter : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Return the cursor location in that view.

   ---------------
   -- Get / Set --
   ---------------

   function Get_Saved_Cursor_Mark
     (View : access Text_View_Record'Class)
      return Gtk_Text_Mark;
   pragma Inline (Get_Saved_Cursor_Mark);

   procedure Set_Saved_Cursor_Mark
     (View   : access Text_View_Record'Class;
      Cursor : Gtk_Text_Mark);
   pragma Inline (Set_Saved_Cursor_Mark);

private

   type Text_View_Record is
     new Gtk.Text_View.Gtk_Text_View_Record
   with record
      Saved_Cursor_Mark : Gtk_Text_Mark;
      --  Copy of the cursor location.

   end record;

end Gtkada.Text_View;
