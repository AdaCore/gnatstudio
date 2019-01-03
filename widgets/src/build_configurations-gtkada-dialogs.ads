------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  This package defines utilities and dialogs used by the GtkAda-based GUI

package Build_Configurations.Gtkada.Dialogs is

   function Models_Combo
     (UI : access Build_UI_Record'Class) return Gtk_Combo_Box_Text;
   --  Create and return a combo box allowing choice between the models in UI

   procedure Add_Target_Dialog
     (UI        : access Build_UI_Record'Class;
      Model     : out Unbounded_String;
      Name      : out Unbounded_String;
      Category  : out Unbounded_String;
      Cancelled : out Boolean);
   --  Launch an "Add target" dialog

   procedure Clone_Target_Dialog
     (UI        : access Build_UI_Record'Class;
      Target    : Target_Access;
      Name      : out Unbounded_String;
      Category  : out Unbounded_String;
      Cancelled : out Boolean);
   --  Launch a "Clone target" dialog

   procedure Delete_Target_Dialog
     (UI        : access Build_UI_Record'Class;
      Target    : Target_Access;
      Cancelled : out Boolean);
   --  Launch a "delete target?" confirmation dialog

   procedure Information
     (UI      : access Build_UI_Record'Class;
      Message : String);
   --  Launch an information dialog containing Message, with just an OK button

   function Yes_No_Dialog
     (UI : access Build_UI_Record'Class;
      M  : String) return Boolean;
   --  Display a dialog for string M that has "Yes" and "No" buttons.
   --  Return True if the "Yes" button was pressed, False otherwise.

end Build_Configurations.Gtkada.Dialogs;
