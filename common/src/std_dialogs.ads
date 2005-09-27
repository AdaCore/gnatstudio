-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                             AdaCore                               --
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

with Gtk.Enums;  use Gtk.Enums;
with Gtk.Window; use Gtk.Window;
with Histories;  use Histories;

package Std_Dialogs is

   function Simple_Entry_Dialog
     (Parent   : access Gtk_Window_Record'Class;
      Title    : String;
      Message  : String;
      Position : Gtk_Window_Position := Win_Pos_Mouse;
      History  : Histories.History := null;
      Key      : History_Key := "") return String;
   --  Open a simple dialog, with a single entry field, and returns the
   --  contents of this field (or ASCII.NUL) if the user selected cancel).
   --  The dialog is set up as a child of Parent, so that, depending on the
   --  window manager, it isn't displayed below it.
   --  if Key is not the empty string, then the combobox's content is
   --  initialized from the corresponding entry in History.
   --  Position indicates where the dialog should be positionned.

   type Boolean_Access is access all Boolean;

   function Display_Entry_Dialog
     (Parent         : access Gtk_Window_Record'Class;
      Title          : String;
      Message        : String;
      Position       : Gtk_Window_Position := Win_Pos_Mouse;
      Check_Msg      : String := "";
      History        : Histories.History;
      Key            : History_Key := "";

      Button_Active  : Boolean_Access := null;
      Key_Check      : History_Key := "";

      Check_Msg2     : String := "";
      Button2_Active : Boolean_Access := null;
      Key_Check2     : History_Key := "") return String;
   --  A dialog, like Simple_Entry_Dialog, specifically set up to enter
   --  expressions to display.

end Std_Dialogs;
