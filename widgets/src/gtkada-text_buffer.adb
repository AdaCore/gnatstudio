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

with Glib;            use Glib;
with Gtk.Text_Iter;   use Gtk.Text_Iter;

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

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      return Gtk.Text_Buffer.Get_Type;
   end Get_Type;

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

end Gtkada.Text_Buffer;
