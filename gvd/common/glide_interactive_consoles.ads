-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package provides the implementation for the GPS interactive console.

with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Text_Mark;

with Gtk.Text_Tag;
with Gtk.Scrolled_Window;

with Glide_Kernel;

package Glide_Interactive_Consoles is

   type Glide_Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Glide_Interactive_Console is
     access all Glide_Interactive_Console_Record'Class;

   procedure Gtk_New
     (Console : out Glide_Interactive_Console;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new console for glide

   procedure Initialize
     (Console : access Glide_Interactive_Console_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization function.

   procedure Clear (Console : access Glide_Interactive_Console_Record);
   --  Clear all the text in the Console.

private

   type Glide_Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
   record
      Kernel : Glide_Kernel.Kernel_Handle;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      View   : Gtk.Text_View.Gtk_Text_View;

      Prompt_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The position after which the user can insert text.

      Insert_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The insert position.

      Internal_Insert : Boolean := False;
      --  Whether internal functions are writing in the console buffer.
      --  Prevents recursion in the handler for "insert_text".

      Uneditable_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag indicating uneditable text.

      Prompt_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used to distinguish the prompt from the rest of the text.

      Button_Press : Boolean := False;
      --  Whether a mouse button is currently pressed.
      --  Used to enable selection in places where the cursor is not allowed.
   end record;

end Glide_Interactive_Consoles;
