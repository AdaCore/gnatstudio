-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gtk.Window;      use Gtk.Window;
with Gtk.Box;         use Gtk.Box;
with Gtk.Label;       use Gtk.Label;
with Gtk.Text;        use Gtk.Text;
with Gtk.Button;      use Gtk.Button;
with String_List;

package Log_Editor_Window_Pkg is

   type Log_Editor_Window_Record is new Gtk_Window_Record with record
      Files_Label   : Gtk_Label;
      Labels_Vbox   : Gtk_Hbox;
      Log_Text      : Gtk_Text;
      Ok_Button     : Gtk_Button;

      Files         : String_List.List;
      --  A list of base file names that correspond to the log.
   end record;
   type Log_Editor_Window_Access is access all Log_Editor_Window_Record'Class;

   procedure Gtk_New (Log_Editor_Window : out Log_Editor_Window_Access);
   --  Create a new Log_Editor_Window_Access.

   procedure Initialize
     (Log_Editor_Window : access Log_Editor_Window_Record'Class);
   --  Internal initialization function.

   procedure Set_Text
     (Log_Editor_Window : access Log_Editor_Window_Record'Class;
      Text              : String);
   --  Set the log text for this editor.

   function Get_Text
     (Log_Editor_Window : access Log_Editor_Window_Record'Class) return String;
   --  Get the log text for this editor.

   procedure Add_File_Name
     (Log_Editor_Window : access Log_Editor_Window_Record'Class;
      File_Name         : String);
   --  Add an entry to the list of file names corresponding to the log.

   procedure Close (Log_Editor_Window : access Log_Editor_Window_Record'Class);
   --  Free memory associated with the log editor and destroy the window.

end Log_Editor_Window_Pkg;
