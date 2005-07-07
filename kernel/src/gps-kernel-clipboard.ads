-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

with Gtk.Widget;
with GNAT.OS_Lib;

package GPS.Kernel.Clipboard is

   type Clipboard_Record is private;
   type Clipboard_Access is access all Clipboard_Record;

   procedure Create_Clipboard
     (Kernel : access Kernel_Handle_Record'Class);
   --  Create a new clipboard in the kernel

   procedure Destroy_Clipboard (Kernel : access Kernel_Handle_Record'Class);
   --  Destroy the clipboard and the memory it uses

   function Get_Clipboard
     (Kernel : access Kernel_Handle_Record'Class) return Clipboard_Access;
   --  Return the clipboard used by GPS. All copy/paste operations should be
   --  done with this keyboard.

   procedure Cut_Clipboard
     (Clipboard        : access Clipboard_Record;
      Widget           : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Cut the current selection from widget to the clipboard. This saves the
   --  previous contents of the clipboard.
   --  Default_Editable indicates whether the buffer is considered as editable
   --  by default.
   --  The selection is deleted from the widget.

   procedure Copy_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Cut the current selection from widget to the clipboard. This saves the
   --  previous contents of the clipboard.

   procedure Paste_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Paste the last clipboard entry made by Cut or Copy (or the one last
   --  pasted by Paste_Previous)

   procedure Paste_Previous_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove the previously pasted text, and replace it by the one before it
   --  in the clipboard. This fails if the current location in the widget is
   --  no the same where Paste_Clipboard last left it.

private
   type Selection_List is
     array (Natural range <>) of GNAT.OS_Lib.String_Access;
   type Selection_List_Access is access Selection_List;

   type Clipboard_Record is record
      Kernel        : Kernel_Handle;
      List          : Selection_List_Access;
      Last_Paste    : Integer;               --  Index in List
      Last_Widget   : Gtk.Widget.Gtk_Widget; --  Where the last paste occurred
      Last_Position : Integer;               --  Where the last paste occurred
   end record;

end GPS.Kernel.Clipboard;
