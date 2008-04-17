-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2005-2008, AdaCore                --
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
with GNAT.Strings;

package GPS.Kernel.Clipboard is

   type Clipboard_Record is private;
   type Clipboard_Access is access all Clipboard_Record;
   pragma No_Strict_Aliasing (Clipboard_Access);

   type Selection_List is
     array (Natural range <>) of GNAT.Strings.String_Access;

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
     (Clipboard : access Clipboard_Record;
      Widget    : access Glib.Object.GObject_Record'Class);
   --  Cut the current selection from widget to the clipboard. This saves the
   --  previous contents of the clipboard.
   --  Default_Editable indicates whether the buffer is considered as editable
   --  by default.
   --  The selection is deleted from the widget.

   procedure Copy_Text_In_Clipboard
     (Clipboard : access Clipboard_Record;
      Text      : String);
   --  Copy a static text into the clipboard. This saves the previous contents
   --  of the clipboard

   procedure Copy_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Glib.Object.GObject_Record'Class);
   --  Copy the current selection from widget to the clipboard. This saves the
   --  previous contents of the clipboard.

   procedure Paste_Clipboard
     (Clipboard     : access Clipboard_Record;
      Widget        : access Glib.Object.GObject_Record'Class;
      Index_In_List : Natural := 0);
   --  Paste the last clipboard entry made by Cut or Copy (or the one last
   --  pasted by Paste_Previous).
   --  By default, the last entry is pasted, but you can force a specific
   --  entry by specifying Index_In_List.

   procedure Paste_Previous_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Remove the previously pasted text, and replace it by the one before it
   --  in the clipboard. This fails if the current location in the widget is
   --  no the same where Paste_Clipboard last left it.

   procedure Merge_Clipboard
     (Clipboard      : access Clipboard_Record;
      Index1, Index2 : Natural);
   --  Merge the two entries in the clipboard, and remove the second one

   procedure Remove_Clipboard_Entry
     (Clipboard : access Clipboard_Record; Index : Natural);
   --  Remove an entry in the clipboard

   function Get_Content
     (Clipboard : access Clipboard_Record) return Selection_List;
   --  Return the current contents of the clipboard. The returned value must
   --  not be freed by the user. Some entries might be set to null in this
   --  list.

   function Get_Last_Paste
     (Clipboard : access Clipboard_Record) return Integer;
   --  Return the index of the last paste text in the result of Get_Context

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register shell commands associated with the clipboard

   Clipboard_Changed_Hook : constant Hook_Name := "clipboard_changed";
   --  Hook called when the contents of the clipboard has changed (either
   --  because we added a new entry to it, or because the index of the last
   --  paste operation has changed).

private
   type Selection_List_Access is access Selection_List;

   type Clipboard_Record is record
      Kernel        : Kernel_Handle;
      List          : Selection_List_Access;
      Last_Paste    : Integer := Integer'Last; --  Index in List
      Last_Widget   : Glib.Object.GObject;   --  Where the last paste occurred
      Last_Position : Integer;               --  Where the last paste occurred
      Last_Length   : Integer;               --  Length of last insert
      Last_Is_From_System : Boolean := False;
      --  True if the last paste was from the system clipboard
   end record;

end GPS.Kernel.Clipboard;
