-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package body GVD.Text_Box.Source_Editor is

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language
     (Editor : access Source_Editor_Record;
      Lang   : Language.Language_Access)
   is
      use Language;
   begin
      Free (Editor.Lang);

      if Lang /= null then
         Editor.Lang := new Language_Root'Class' (Lang.all);
      end if;
   end Set_Current_Language;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Source_Editor_Record) return String
   is
      use type GVD.Types.String_Access;
   begin
      if Editor.Current_File = null then
         return "";
      else
         return Editor.Current_File.all;
      end if;
   end Get_Current_File;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (Editor : access Source_Editor_Record) return Gtk.Widget.Gtk_Widget is
   begin
      return Editor.Widget;
   end Get_Widget;

end GVD.Text_Box.Source_Editor;
