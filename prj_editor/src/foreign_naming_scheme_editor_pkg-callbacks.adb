-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Types; use Gdk.Types;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Types; use Gtkada.Types;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Foreign_Naming_Editors;  use Foreign_Naming_Editors;
with Glide_Intl; use Glide_Intl;

package body Foreign_Naming_Scheme_Editor_Pkg.Callbacks is

   Empty_Filename : constant String := "<filename>";

   procedure Handle_Key (E : Foreign_Naming_Editor; Event : Gdk_Event);
   --  Handle a keypress (escape,...) in Field

   use Gtk.Arguments;

   ----------------------------------
   -- On_Exception_List_Select_Row --
   ----------------------------------

   procedure On_Exception_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Row       : constant Gint := To_Gint (Params, 1);
      E         : constant Foreign_Naming_Editor :=
        Foreign_Naming_Editor (Object);
      File_Name : constant String := Get_Text (E.Exception_List, Row, 0);
   begin
      Set_Text (E.Filename_Entry, File_Name);
   end On_Exception_List_Select_Row;

   ---------------------------------------
   -- On_Exception_List_Key_Press_Event --
   ---------------------------------------

   function On_Exception_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
      use Gint_List;
      E    : constant Foreign_Naming_Editor := Foreign_Naming_Editor (Object);
      List : constant Gint_List.Glist := Get_Selection (E.Exception_List);
   begin
      if Get_Key_Val (Arg1) = GDK_Delete then
         if List /= Null_List then
            Remove (E.Exception_List, Get_Data (List));
         end if;
         return True;
      end if;
      return False;
   end On_Exception_List_Key_Press_Event;

   -----------------------
   -- On_Update_Clicked --
   -----------------------

   procedure On_Update_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      E    : constant Foreign_Naming_Editor := Foreign_Naming_Editor (Object);
      Text : Gtkada.Types.Chars_Ptr_Array (0 .. 0);
      Num_Rows : constant Gint := Get_Rows (E.Exception_List);
      File : constant String := Get_Text (E.Filename_Entry);
      Row : Gint := -1;
   begin
      if File /= -Empty_Filename then
         --  Check if there is already an entry for this file
         for J in 0 .. Num_Rows - 1 loop
            declare
               U : constant String := Get_Text (E.Exception_List, J, 0);
            begin
               if U = File then
                  Row := J;
               elsif  U > File then
                  exit;
               end if;
            end;
         end loop;

         if Row = -1 then
            Text (0) := New_String (Get_Text (E.Filename_Entry));
            Row := Append (E.Exception_List, Text);
            Free (Text);
         end if;

         Moveto (E.Exception_List, Row, 0, 0.0, 0.0);
         Set_Text (E.Filename_Entry, -Empty_Filename);
         Grab_Focus (E.Filename_Entry);
      end if;
   end On_Update_Clicked;

   ----------------
   -- Handle_Key --
   ----------------

   procedure Handle_Key (E : Foreign_Naming_Editor; Event : Gdk_Event) is
   begin
      if Get_Key_Val (Event) = GDK_Escape then
         Set_Text (E.Filename_Entry, -Empty_Filename);
      end if;
   end Handle_Key;

   ---------------------------------------
   -- On_Filename_Entry_Key_Press_Event --
   ---------------------------------------

   function On_Filename_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
      E    : constant Foreign_Naming_Editor := Foreign_Naming_Editor (Object);
   begin
      if Get_Text (E.Filename_Entry) = -Empty_Filename then
         Set_Text (E.Filename_Entry, "");
      end if;
      Handle_Key (E, Arg1);
      return False;
   end On_Filename_Entry_Key_Press_Event;

end Foreign_Naming_Scheme_Editor_Pkg.Callbacks;
