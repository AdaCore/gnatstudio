------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

--  This package provides filter panel.
--  Instances of this panel emit Signal_Filter_Changed signal which is used
--  for notify that content of a filter has been changed.

with Glib;

with Gtk.Check_Menu_Item;  use Gtk.Check_Menu_Item;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Radio_Menu_Item;  use Gtk.Radio_Menu_Item;
with Gtk.Tool_Item;        use Gtk.Tool_Item;
with Gtk.Widget;           use Gtk.Widget;

with Gtkada.Search_Entry;  use Gtkada.Search_Entry;
with GPS.Kernel;           use GPS.Kernel;
with GPS.Search;           use GPS.Search;
with Histories;            use Histories;

private with Ada.Strings.Unbounded;
private with GNAT.Strings;
private with Glib.Main;

package Filter_Panels is

   type Filter_Options_Mask is mod Natural'Last;
   Has_Regexp      : constant Filter_Options_Mask := 2 ** 0;
   Has_Negate      : constant Filter_Options_Mask := 2 ** 1;
   Has_Whole_Word  : constant Filter_Options_Mask := 2 ** 2;
   Has_Approximate : constant Filter_Options_Mask := 2 ** 3;
   Has_Fuzzy       : constant Filter_Options_Mask := 2 ** 4;
   Debounce        : constant Filter_Options_Mask := 2 ** 5;
   --  If Debouncs is set, then all changes to the filter are reported when the
   --  user presses <enter>. Otherwise, they are reported for all changes to
   --  the pattern, as they occur.

   type Filter_Panel_Record is
     new Gtk.Tool_Item.Gtk_Tool_Item_Record with private;
   type Filter_Panel is access all Filter_Panel_Record'Class;

   procedure Gtk_New
     (Panel       : out Filter_Panel;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Hist_Prefix : Histories.History_Key;
      Tooltip     : String := "";
      Placeholder : String := "";
      Options     : Filter_Options_Mask := 0;
      Name        : String := "");

   procedure Initialize
     (Self        : not null access Filter_Panel_Record'Class;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Hist_Prefix : Histories.History_Key;
      Tooltip     : String := "";
      Placeholder : String := "";
      Options     : Filter_Options_Mask := 0;
      Name        : String := "");

   --  Create a filter panel which provides a standard look-and-feel:
   --     * rounded corner (through the theme)
   --     * "clear" icon
   --     * placeholder text
   --     * tooltip
   --     * a number of predefined options
   --     * remember option settings across sessions (through Hist_Prefix)
   --     * name is used to access filter from testsuite
   --  Whenever the pattern is changed (or cleared), Self.Filter_Changed is
   --  called.
   --  Nothing is done if the filter panel has already been built.
   --  This function should be called from Create_Toolbar.

   procedure Set_Filter
     (Self : not null access Filter_Panel_Record;
      Text : String);
   --  Change the text of the filter

   function Get_Focus_Widget
     (Self : not null access Filter_Panel_Record'Class)
      return Gtk_Widget;
   --  Returns a widget which should be focused by default

   function Get_Filter_Pattern
     (Self : not null access Filter_Panel_Record'Class)
      return Search_Pattern_Access;
   --  Returns current filter pattern

   Signal_Filter_Changed : constant Glib.Signal_Name;
   --  This signal emitted by the filter in the case of the criteria change

private
   type Filter_Panel_Record is new Gtk.Tool_Item.Gtk_Tool_Item_Record
     with record
      Pattern             : Gtkada.Search_Entry.Gtkada_Search_Entry;
      Options             : Filter_Options_Mask;
      Pattern_Config_Menu : Gtk.Menu.Gtk_Menu;

      Kernel              : access GPS.Kernel.Kernel_Handle_Record'Class;
      History_Prefix      : GNAT.Strings.String_Access;
      --  Prefix for the entries in the histories.ads API

      Whole_Word          : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Negate              : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
      Full_Text           : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
      Regexp              : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
      Fuzzy               : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;
      Approximate         : Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item;

      Timeout             : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;

      --  Current data for building filter pattern

      Data_Pattern         : Ada.Strings.Unbounded.Unbounded_String;
      Data_Whole_Word      : Boolean     := False;
      Data_Negate          : Boolean     := False;
      Data_Kind            : Search_Kind := GPS.Search.Full_Text;
     end record;

   function Report_Filter_Changed_Idle
     (Self : Filter_Panel) return Boolean;

   procedure Store_Filter_Data
     (Self : not null access Filter_Panel_Record'Class);
   --  Stores data for building filter

   procedure Update_Recent_Entries
     (Panel : not null access Filter_Panel_Record'Class;
      Add   : access Search_Pattern'Class := null);
   --  If Add is specified, add it to the search pattern to the history.
   --  In all cases, update the menu to show all recent entries.

   Signal_Filter_Changed : constant Glib.Signal_Name := "filter-changed";

end Filter_Panels;
