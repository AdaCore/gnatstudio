-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.CList;           use Gtk.CList;
with Gtk.Label;           use Gtk.Label;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Button;          use Gtk.Button;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

package List_Select_Pkg is

   --  This package provides a simple selectable list dialog.
   --
   --  Typical usage consists in calling Gtk_New,
   --  then using Add_Item as many times as required to fill the dialog,
   --  then calling Show to effectively display the dialog.
   --
   --  See gvd-main_debug_window_pkg-callbacks.adb for an example.

   type List_Select_Record is new Gtk_Window_Record with private;
   type List_Select_Access is access all List_Select_Record'Class;

   procedure Gtk_New
     (List_Select : out List_Select_Access;
      Title         : String := "";
      Help_Message  : String := "";
      Item_Label    : String := "";
      Comment_Label : String := "");
   --  Create a new List_Select dialog.
   --  Title is the title of the window.
   --  Help_Message is the text that will be displayed when pressing Help
   --  (if Help_Message is left to "", then the Help button is not displayed.)
   --  Item_Label is the label on top of the first column in the list.

   procedure Initialize
     (List_Select   : access List_Select_Record'Class;
      Title         : String;
      Help_Message  : String;
      Item_Label    : String;
      Comment_Label : String);
   --  Internal initialize procedure.

   procedure Add_Item
     (List_Select : List_Select_Access;
      Label       : String;
      Comment     : String);
   --  Add an item to the list with the specified label.

   procedure Remove_All_Items (List_Select : List_Select_Access);
   --  Removes all the items in the list.

   function Show
     (List_Select : List_Select_Access) return String;
   --  Displays the list until OK or Cancel is pressed.
   --  Upon exit, returns the contents of the entry.

private
   type List_Select_Record is new Gtk_Window_Record with record
      Help_Text      : String_Access;

      Vbox           : Gtk_Vbox;
      Hbox           : Gtk_Hbox;
      Scrolledwindow : Gtk_Scrolled_Window;
      List           : Gtk_Clist;
      Label1         : Gtk_Label;
      Label2         : Gtk_Label;
      Hbox2          : Gtk_Hbox;
      The_Entry      : Gtk_Entry;
      Hbuttonbox     : Gtk_Hbutton_Box;
      Ok             : Gtk_Button;
      Cancel         : Gtk_Button;
      Help           : Gtk_Button;
   end record;
end List_Select_Pkg;
