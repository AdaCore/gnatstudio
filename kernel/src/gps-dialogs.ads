------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  Support for dialogs.
--  This package provides various helpers to create dialogs, as well as a few
--  standard dialogs reused in various places

with Glib;               use Glib;
with Gtk.Check_Button;   use Gtk.Check_Button;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Enums;          use Gtk.Enums;
with GPS.Kernel;         use GPS.Kernel;
with Histories;          use Histories;

package GPS.Dialogs is

   -------------
   -- Dialogs --
   -------------

   type GPS_Dialog_Record is new Gtk_Dialog_Record with record
      Kernel : access Kernel_Handle_Record'Class;
   end record;
   type GPS_Dialog is access all GPS_Dialog_Record'Class;
   --  All dialogs in GPS should either be full MDI_Child or derived from the
   --  type GPS_Dialog. This type ensures that when the dialogs gets the
   --  focus, the current context is properly updated in the kernel.
   --  This type also ensures consistency in the use of the header bar for the
   --  action buttons.
   --  These dialogs will also automatically save/restore their size when they
   --  are launched.

   procedure Gtk_New
     (Self           : out GPS_Dialog;
      Title          : Glib.UTF8_String;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Flags          : Gtk_Dialog_Flags := Destroy_With_Parent;
      Typ            : Glib.GType := Gtk.Dialog.Get_Type;
      Default_Width  : Glib.Gint := -1;
      Default_Length : Glib.Gint := -1);
   procedure Initialize
     (Self           : not null access GPS_Dialog_Record'Class;
      Title          : Glib.UTF8_String;
      Kernel         : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Flags          : Gtk_Dialog_Flags := Destroy_With_Parent;
      Typ            : Glib.GType := Gtk.Dialog.Get_Type;
      Default_Width  : Glib.Gint := -1;
      Default_Length : Glib.Gint := -1);
   --  Create a new empty dialog.
   --
   --  If you are subclassing the GtkDialog class to add new signals, pass the
   --  id of the new class in the Typ parameter, so that the proper gtk+
   --  widget is allocated.

   --------------------
   -- Action buttons --
   --------------------

   procedure Add_OK_Cancel (Self : not null access GPS_Dialog_Record'Class);
   --  Add [OK] and [Cancel] buttons, in the proper order.
   --  OK is always set as the default action for the dialog

   procedure Add_Button
     (Self       : not null access GPS_Dialog_Record'Class;
      Text       : String;
      Response   : Gtk_Response_Type;
      Is_Default : Boolean := False);
   --  Add a button with custom text

   -----------------
   -- Combo boxes --
   -----------------

   type Combo_Box_Record is tagged private;
   type Combo_Box is access all Combo_Box_Record'Class;
   function Add_Combo
     (Self    : not null access GPS_Dialog_Record'Class;
      Message : String;
      Key     : Histories.History_Key) return Combo_Box;
   --  Add a new combo box in the dialog, below all the others that were
   --  added previously.

   function Get_Text (Self : not null access Combo_Box_Record) return String;
   --  Return the value from the combo box, and store it in the history.

   -------------------
   -- Check buttons --
   -------------------

   function Add_Check_Button
     (Self    : not null access GPS_Dialog_Record'Class;
      Message : String;
      Key     : Histories.History_Key) return Gtk_Check_Button;
   --  Add a new check box in the dialog, below all the others that were
   --  added previously.
   --  The check box's value is stored in history so that it is set to
   --  the same value in the future.

   -----------
   -- Label --
   -----------

   procedure Add_Label
     (Self    : not null access GPS_Dialog_Record'Class;
      Message : String);
   --  Add a new static label to the dialog

   ----------------
   -- Text_Input --
   ----------------

   function Display_Text_Input_Dialog
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Title          : String;
      Message        : String;
      Key            : History_Key := "";
      Check_Msg      : String := "";
      Button_Active  : access Boolean := null;
      Key_Check      : Histories.History_Key := "";
      Check_Msg2     : String := "";
      Button2_Active : access Boolean := null;
      Key_Check2     : Histories.History_Key := "") return String;
   --  A simple wrapper for the subprograms above.
   --  This allows simple creation of a dialog with:
   --       message:  [       v]
   --       [x] check_msg (optional)
   --       [x] check_msg2 (optional)
   --  This displays the dialog, and waits for the user to press [OK] (in
   --  which case it returns the value of the combo box) or [Cancel] (in which
   --  case is returns a string with one ASCII.NUL only).

   generic
      type Enumerated_Type is (<>);

   function Display_Select_Dialog
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Title   : String;
      Message : String;
      Value   : in out Enumerated_Type) return Boolean;
   --  Like as above but with a combobox for a value selection.
   --  This displays the dialog, and waits for the user to press [OK] (in
   --  which case it returns True and the Value) or [Cancel] (in which case is
   --  returns False).

private

   type History_Key_Access is access all Histories.History_Key;

   type Combo_Box_Record is new Gtk_Combo_Box_Text_Record with record
      Kernel : access Kernel_Handle_Record'Class;
      Key    : History_Key_Access;
   end record;

end GPS.Dialogs;
