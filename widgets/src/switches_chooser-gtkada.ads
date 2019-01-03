------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Gtk.GEntry;
with Gtk.Box;
with Gtk.Widget;

with Histories;  use Histories;
with Pango.Font; use Pango.Font;

package Switches_Chooser.Gtkada is

   package Gtk_Switches_Editors is new Switches_Editors
     (Gtk.Widget.Gtk_Widget_Record,
      Gtk.Box.Gtk_Vbox_Record);

   type Switches_Editor_Record is new Gtk_Switches_Editors.Root_Switches_Editor
   with private;
   type Switches_Editor is access all Switches_Editor_Record'Class;

   procedure Gtk_New
     (Editor             : out Switches_Editor;
      Config             : Switches_Editor_Config;
      Use_Native_Dialogs : Boolean;
      Read_Only          : Boolean;
      History            : Histories.History;
      Key                : History_Key;
      Cmd_Line_Tooltip   : String;
      Help_Msg           : String := "";
      Fixed_Font         : Pango_Font_Description := null);
   procedure Initialize
     (Editor             : access Switches_Editor_Record'Class;
      Config             : Switches_Editor_Config;
      Use_Native_Dialogs : Boolean;
      Read_Only          : Boolean;
      History            : Histories.History;
      Key                : History_Key;
      Cmd_Line_Tooltip   : String;
      Help_Msg           : String := "";
      Fixed_Font         : Pango_Font_Description := null);
   --  Create a new switches editor based on Config.
   --  Use_Native_Dialogs applies to the file selector and directory selector
   --  dialogs
   --  Cmd_Line_Tooltip is the string that will be displayed when hovering the
   --  command line editor.

   function Get_Entry
     (Editor : access Switches_Editor_Record'Class)
      return Gtk.GEntry.Gtk_Entry;
   --  Return the switches entry.

private
   type Widget_Array is array (Natural range <>) of Gtk.Widget.Gtk_Widget;
   type Widget_Array_Access is access Widget_Array;

   overriding procedure Set_Graphical_Command_Line
     (Editor    : in out Switches_Editor_Record; Cmd_Line  : String);
   overriding procedure Set_Graphical_Widget
     (Editor     : in out Switches_Editor_Record;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Switch     : Switch_Type;
      Parameter  : String;
      Is_Default : Boolean := False);

   type Switches_Editor_Record is new Gtk_Switches_Editors.Root_Switches_Editor
   with record
      Native_Dialogs : Boolean;
      Ent            : Gtk.GEntry.Gtk_Entry;
      Read_Only      : Boolean;
   end record;

end Switches_Chooser.Gtkada;
