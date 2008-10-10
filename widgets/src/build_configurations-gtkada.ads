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

with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo_Box;    use Gtk.Combo_Box;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Tooltips;     use Gtk.Tooltips;
with Gtk.Window;       use Gtk.Window;

with Gtkada.Tree_View;         use Gtkada.Tree_View;

with Histories;                use Histories;
with Switches_Chooser.Gtkada;  use Switches_Chooser.Gtkada;

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

   type Cmd_Line_Expander is access function (CL : String) return String;

   procedure Single_Target_Dialog
     (Registry        : Build_Config_Registry_Access;
      Parent          : Gtk_Window   := null;
      Tooltips        : Gtk_Tooltips := null;
      Target          : String;
      History         : Histories.History;
      Expand_Cmd_Line : Cmd_Line_Expander;
      Result          : out GNAT.OS_Lib.Argument_List_Access);
   --  Launch a dialog allowing to modify the command line for Target only.
   --  Return the resulting command followed by arguments, macros not
   --  expanded.
   --  Use History to prefill the dialog.
   --  Result is set to null if the user canceled the dialog, otherwise to the
   --  the unexpanded command line.
   --  Expand_Cmd_Line will be used to expand meta characters in a command
   --  line.

private

   type Target_UI_Record is new Gtk_Hbox_Record with record
      Registry       : Build_Config_Registry_Access;
      Target         : Target_Access;

      Tooltips       : Gtk_Tooltips;

      Frame          : Gtk_Frame;
      --  The frame that contains the elements to describe the switches

      Editor         : Switches_Editor := null;
      --  The one switch editor for the target, if there is only one command

      Model_Entry    : Gtk_Entry;
      --  The entry containing the model

      History        : Histories.History;

      Icon_Entry     : Gtk_Entry;
      Icon_Check     : Gtk_Check_Button;
      Launch_Combo   : Gtk_Combo_Box;

      Expanded_Entry : Gtk_Entry;
   end record;
   type Target_UI_Access is access all Target_UI_Record'Class;

   type Build_UI_Record is new Gtk_Hbox_Record with record
      Registry  : Build_Config_Registry_Access;
      Target_UI : Target_UI_Access;
      --  Single target UI when using the Single mode

      Expand_Cmd_Line : Cmd_Line_Expander;
      --  Command line expander callback

      Notebook  : Gtk_Notebook;
      --  The main notebook

      Tooltips  : Gtk_Tooltips;
      --  The tooltips used in the dialog

      View      : Tree_View;
      --  The tree

      History   : Histories.History;
      --  Reference to the History
   end record;

end Build_Configurations.Gtkada;
