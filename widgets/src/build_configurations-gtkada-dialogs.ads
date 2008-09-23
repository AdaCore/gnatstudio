-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package defines utilities and dialogs used by the GtkAda-based GUI

with Gtk.Combo_Box_Entry; use Gtk.Combo_Box_Entry;

package Build_Configurations.Gtkada.Dialogs is

   function Models_Combo
     (UI : access Build_UI_Record'Class) return Gtk_Combo_Box_Entry;
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

end Build_Configurations.Gtkada.Dialogs;
