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

with Gtk.Box;            use Gtk.Box;
with Gtk.Check_Button;   use Gtk.Check_Button;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Window;         use Gtk.Window;
with Histories;          use Histories;

package Std_Dialogs is

   type Text_Input_Dialog_Record is new Gtk_Dialog_Record with private;
   type Text_Input_Dialog is access
     all Text_Input_Dialog_Record'Class;
   --  Base type for all the text input dialogs

   procedure Initialize
     (Self       : not null access Text_Input_Dialog_Record'Class;
      Parent     : access Gtk.Window.Gtk_Window_Record'Class;
      Title      : String;
      Message    : String;
      Position   : Gtk_Window_Position := Win_Pos_Center_On_Parent;
      History    : Histories.History;
      Key        : History_Key := "";
      Check_Msg  : String := "";
      Key_Check  : Histories.History_Key := "";
      Check_Msg2 : String := "";
      Key_Check2 : Histories.History_Key := "");
   --  Initialize the common attributes of text input dialogs.
   --
   --  If Parent is specified, it is set as the transient window for the
   --  dialog.
   --
   --  Position indicates where the dialog shoudl be positioned.

   --  If non-empty values are given for Kistory and Key, then the text input
   --  field content is initialized from the corresponding entry in History.
   --
   --  The Check_Msg and Key_Check optional parameters can be used to display
   --  additional checkboxes under the text input field, saving the state
   --  of each checkbox in their corresponding entry in History.

   function Run_And_Get_Input
     (Self           : not null access Text_Input_Dialog_Record'Class;
      History        : Histories.History;
      Key            : History_Key;
      Button_Active  : access Boolean := null;
      Button_Active2 : access Boolean := null) return String;
   --  Run the text input dialog and return the input typed by the user.
   --
   --  If the text input dialog has been initialized with extra checkboxes,
   --  Button_Active and Button_Active2 are set to their active state after
   --  clicking on the 'Ok' button.

private

   type Text_Input_Dialog_Record is new Gtk_Dialog_Record with record
      Main_View    : Gtk_Vbox;
      --  The text input dialog's main view

      Combo_Entry  : Gtk_Combo_Box_Text;
      --  The text input field

      Check        : Gtk_Check_Button;
      --  First optional check box

      Check2       : Gtk_Check_Button;
      --  Second optional check box
   end record;

end Std_Dialogs;
