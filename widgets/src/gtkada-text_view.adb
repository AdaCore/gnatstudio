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

with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Mark; use Gtk.Text_Mark;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Widget; use Gtk.Widget;

with Gtkada.Text_Buffer; use Gtkada.Text_Buffer;

package body Gtkada.Text_View is

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (View : access Text_View_Record'Class;
      Iter : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
   begin
      if Has_Focus_Is_Set (View) then
         Get_Cursor_Position (Text_Buffer_Access (Get_Buffer (View)), Iter);
      else
         Get_Iter_At_Mark (Get_Buffer (View), Iter, View.Saved_Cursor_Mark);
      end if;
   end Get_Cursor_Position;

   ---------------------------
   -- Get_Saved_Cursor_Mark --
   ---------------------------

   function Get_Saved_Cursor_Mark
     (View : access Text_View_Record'Class)
      return Gtk_Text_Mark is
   begin
      return View.Saved_Cursor_Mark;
   end Get_Saved_Cursor_Mark;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Text_View_Access;
      Buffer : Text_Buffer_Access := null) is
   begin
      View := new Text_View_Record;
      Initialize (View, Buffer);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Text_View_Record'Class;
      Buffer : Text_Buffer_Access) is
   begin
      Gtk.Text_View.Initialize (View, Gtk_Text_Buffer (Buffer));
   end Initialize;

   --------------------------
   -- Save_Cursor_Position --
   --------------------------

   procedure Save_Cursor_Position (View : access Text_View_Record'Class) is
      Buffer : constant Text_Buffer_Access :=
                 Text_Buffer_Access (Get_Buffer (View));
      Insert_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      Move_Mark (Buffer, View.Saved_Cursor_Mark, Insert_Iter);
   end Save_Cursor_Position;

   ---------------------------
   -- Set_Saved_Cursor_Mark --
   ---------------------------

   procedure Set_Saved_Cursor_Mark
     (View   : access Text_View_Record'Class;
      Cursor : Gtk_Text_Mark) is
   begin
      View.Saved_Cursor_Mark := Cursor;
   end Set_Saved_Cursor_Mark;

end Gtkada.Text_View;
