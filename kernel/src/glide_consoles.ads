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

--  This package provides the implementation for the glide console.
--  It includes all the functions related to manipulating the console.

with Gtk.Scrolled_Window;
with Gtk.Text;
with Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;

package Glide_Consoles is

   type Glide_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Glide_Console is access all Glide_Console_Record'Class;

   procedure Gtk_New
     (Console : out Glide_Console;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new console for glide

   procedure Initialize
     (Console : access Glide_Console_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization function.

   procedure Insert
     (Console        : access Glide_Console_Record;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info);
   --  Insert Text in the Glide's console.
   --  Highlight parts of Text that match a source location (the color is set
   --  using the preferences) if Highlight_Sloc is True.
   --  If Add_LF is True, automatically add a line separator.

   procedure Clear (Console : access Glide_Console_Record);
   --  Clear all the text in the Console.

private

   type Glide_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Kernel : Glide_Kernel.Kernel_Handle;
         Text   : Gtk.Text.Gtk_Text;
      end record;

end Glide_Consoles;
