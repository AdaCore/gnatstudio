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

with Glib;          use Glib;
with Gtk.Text_Iter; use Gtk.Text_Iter;

package body Gtkada.Text_Buffer is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Buffer : out Gtkada_Text_Buffer) is
   begin
      Buffer := new Gtkada_Text_Buffer_Record;
      Initialize (Buffer);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Buffer : access Gtkada_Text_Buffer_Record'Class) is
   begin
      Gtk.Text_Buffer.Initialize (Buffer);
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : access Gtkada_Text_Buffer_Record'Class) is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if Get_Char_Count (Buffer) > 0 then
         Get_Bounds (Buffer, Start_Iter, End_Iter);
         Delete (Buffer, Start_Iter, End_Iter);
      end if;
   end Clear;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (Buffer : access Gtkada_Text_Buffer_Record'Class;
      Iter   : out Gtk_Text_Iter) is
   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
   end Get_Cursor_Position;

   ---------------------
   -- Get_Insert_Mark --
   ---------------------

   function Get_Insert_Mark
     (Buffer : access Gtkada_Text_Buffer_Record'Class)
      return Gtk_Text_Mark is
   begin
      return Buffer.Insert_Mark;
   end Get_Insert_Mark;

   ---------------------
   -- Set_Insert_Mark --
   ---------------------

   procedure Set_Insert_Mark
     (Buffer : access Gtkada_Text_Buffer_Record'Class;
      Mark   : Gtk_Text_Mark) is
   begin
      Buffer.Insert_Mark := Mark;
   end Set_Insert_Mark;

end Gtkada.Text_Buffer;
