------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Gtk.Enums;  use Gtk.Enums;
with Gtk.Window; use Gtk.Window;
with Histories;  use Histories;

package Std_Dialogs is

   type Boolean_Access is access all Boolean;

   function Display_Text_Input_Dialog
     (Parent         : access Gtk_Window_Record'Class;
      Title          : String;
      Message        : String;
      Position       : Gtk_Window_Position := Win_Pos_Center_On_Parent;
      Multi_Line     : Boolean := False;
      History        : Histories.History := null;
      Key            : History_Key := "";
      Check_Msg      : String := "";
      Button_Active  : Boolean_Access := null;
      Key_Check      : Histories.History_Key := "";
      Check_Msg2     : String := "";
      Button2_Active : Boolean_Access := null;
      Key_Check2     : Histories.History_Key := "") return String;
   --  Display a simple text input dialog and returns the contents of the
   --  text input field (or ASCII.NUL if the user selected cancel).
   --
   --  The dialog is set up as a child of Parent, so that, depending on the
   --  window manager, it isn't displayed below it.

   --  If Multi_Line is True, a text view is displayed instead of an entry for
   --  the text input field.

   --  if non-empty values are given for Kistory and Key, then the text input
   --  field content is initialized from the corresponding entry in History.
   --
   --  Position indicates where the dialog should be positionned.
   --
   --  The Check_Msg, Button_Active and Key_Check optional parameters can be
   --  used to display additional checkboxes under the text input field, saving
   --  the state of each checkbox in their corresponding entry in History.

private

   type Text_Input_Dialog_Interface is interface;
   type Text_Input_Dialog_Interface_Access is
     access all Text_Input_Dialog_Interface'Class;
   --  Interface for all the text input dialogs

   procedure Initialize
     (Self    : not null access Text_Input_Dialog_Interface;
      Message : String;
      History : Histories.History;
      Key     : Histories.History_Key) is abstract;
   --  Initialize the given text input dialog, retrieving the previous input
   --  from History and Key.

   function Get_Input_Text
     (Self : not null access Text_Input_Dialog_Interface)
      return String is abstract;
   --  Return the text input by the user

end Std_Dialogs;
