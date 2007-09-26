-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007, AdaCore                  --
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

with Gtk.GEntry;
with Gtk.Table;
with Gtk.Tooltips;
with Gtk.Widget;

package Switches_Chooser.Gtkada is

   package Gtk_Switches_Editors is new Switches_Editors
     (Gtk.Widget.Gtk_Widget_Record, Gtk.Table.Gtk_Table_Record);

   type Switches_Editor_Record is new Gtk_Switches_Editors.Root_Switches_Editor
   with private;
   type Switches_Editor is access all Switches_Editor_Record'Class;

   procedure Gtk_New
     (Editor             : out Switches_Editor;
      Config             : Switches_Editor_Config;
      Tooltips           : Gtk.Tooltips.Gtk_Tooltips;
      Use_Native_Dialogs : Boolean);
   procedure Initialize
     (Editor             : access Switches_Editor_Record'Class;
      Config             : Switches_Editor_Config;
      Tooltips           : Gtk.Tooltips.Gtk_Tooltips;
      Use_Native_Dialogs : Boolean);
   --  Create a new switches editor based on Config.
   --  Use_Native_Dialogs applies to the file selector and directory selector
   --  dialogs

private
   type Widget_Array is array (Natural range <>) of Gtk.Widget.Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   overriding procedure Set_Graphical_Command_Line
     (Editor    : in out Switches_Editor_Record; Cmd_Line  : String);
   overriding procedure Set_Graphical_Widget
     (Editor    : in out Switches_Editor_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Switch    : Switch_Type;
      Parameter : String);

   type Switches_Editor_Record is new Gtk_Switches_Editors.Root_Switches_Editor
   with record
      Native_Dialogs : Boolean;
      Tooltips       : Gtk.Tooltips.Gtk_Tooltips;
      Ent            : Gtk.GEntry.Gtk_Entry;
   end record;

end Switches_Chooser.Gtkada;
