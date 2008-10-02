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

--  This package implements a GtkAda-based GUI for managing build
--  configurations.
--
--  It is intended to depend on GtkAda but not on GPS.

with Gtk.Box;                  use Gtk.Box;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Tooltips;             use Gtk.Tooltips;
with Gtk.Window;               use Gtk.Window;

with Gtkada.Tree_View;         use Gtkada.Tree_View;

with Histories;                use Histories;

package Build_Configurations.Gtkada is

   type Build_UI_Record is new Gtk_Hbox_Record with private;
   type Build_UI_Access is access all Build_UI_Record'Class;

   procedure Configuration_Dialog
     (Registry     : Build_Config_Registry_Access;
      Parent       : Gtk_Window   := null;
      Tooltips     : Gtk_Tooltips := null;
      Changes_Made : out Boolean);
   --  Launch the full configuration dialog
   --  Changes_Made is set to True if the user caused some changes that
   --  need to be saved (in other words, if the  user clicked "OK" or "Apply").

   procedure Single_Target_Dialog
     (Registry : Build_Config_Registry_Access;
      Parent   : Gtk_Window   := null;
      Tooltips : Gtk_Tooltips := null;
      Target   : String;
      History  : in out History_Record;
      Result   : out GNAT.OS_Lib.Argument_List_Access);
   --  Launch a dialog allowing to modify the command line for Target only.
   --  Return the resulting command followed by arguments, macros not
   --  expanded.
   --  Use History to prefill the dialog.
   --  Result is set to null if the user canceled the dialog, otherwise to the
   --  the unexpanded command line.

private

   type Build_UI_Record is new Gtk_Hbox_Record with record
      Registry : Build_Config_Registry_Access;

      Notebook : Gtk_Notebook;
      --  The main notebook

      Tooltips : Gtk_Tooltips;
      --  The tooltips used in the dialog

      View     : Tree_View;
      --  The tree
   end record;

end Build_Configurations.Gtkada;
