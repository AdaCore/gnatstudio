-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Find_Utils;        use Find_Utils;
with GPS.Kernel;        use GPS.Kernel;
with Gtk.Button;        use Gtk.Button;
with Gtk.Main;          use Gtk.Main;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Box;           use Gtk.Box;
with Gtk.Label;         use Gtk.Label;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Table;         use Gtk.Table;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Combo;         use Gtk.Combo;
with Collapsing_Pane;   use Collapsing_Pane;
with Gdk.Pixbuf;
with GNAT.OS_Lib;

--  This package provides an extended version of the visual search
--  widget that can be found in module vsearch, so that it can be integrated
--  within the project explorer directly.

package Vsearch is

   type Vsearch_Record is new Gtk_Box_Record with private;
   type Vsearch_Access is access all Vsearch_Record'Class;

   procedure Gtk_New
     (New_Vsearch : out Vsearch_Access;
      Handle      : GPS.Kernel.Kernel_Handle);
   --  Create a new extended search dialog.

   procedure Initialize
     (Vsearch : access Vsearch_Record'Class;
      Handle  : GPS.Kernel.Kernel_Handle);
   --  Internal initialization procedure.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Register_Search_Function
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : Find_Utils.Search_Module_Data);
   --  See Find_Utils.Register_Search_Function;

   procedure Register_Preferences (Kernel : access Kernel_Handle_Record'Class);
   --  Register the preferences associated to the search functions

   function Search_Context_From_Module
     (Id     : access GPS.Kernel.Abstract_Module_ID_Record'Class;
      Handle : access Kernel_Handle_Record'Class)
      return Find_Utils.Search_Module_Data;
   --  See Find_Utils.Context_From_Module;

   procedure Set_Last_Of_Module
     (Handle      : access Kernel_Handle_Record'Class;
      Search_Data : Find_Utils.Search_Module_Data);
   --  The Search_Data given in parameter is set as beign the last one selected
   --  by the user, and will be the next one shown for the corresponding
   --  module.

   ---------------------
   -- Search Patterns --
   ---------------------
   --  This module defines the following extension point in the custom files:
   --    <vsearch-pattern>
   --       <name>Name displayed in the combo box</name>
   --       <regexp case-sensitive="false" >Replacement regexp</regexp>
   --       <string case-sensitive="false" >Replacement string</string>
   --    </vsearch-pattern>
   --
   --  Only one of regexp or string should be specified. The default is to
   --  have case-insensitive patterns. If multiple regexps or strings are
   --  given, the first regexp found will be used, and if there is none the
   --  first string.
   --
   --  You should use the interface in GPS.Kernel.Custom instead of directly
   --  calling the subprograms below to limit the dependencies on this package

   procedure Register_Search_Pattern
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Regexp         : String;
      Case_Sensitive : Boolean := False;
      Is_Regexp      : Boolean := True);
   --  Register a new template regular expression in the search engine.
   --  Name will appear in the popdown menu of the combo box, but this will be
   --  associated with the regular expression Regexp.
   --  This emits the "search_regexps_changed" signal on Kernel.

   function Search_Regexps_Count
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Natural;
   --  Return the number of registered predefined patterns

   procedure Get_Nth_Search_Regexp_Options
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Num            : Natural;
      Case_Sensitive : out Boolean;
      Is_Regexp      : out Boolean);
   --  Return the options for the Num-th predefined search regexp

   function Get_Nth_Search_Regexp_Name
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class; Num : Natural)
      return String;
   --  Return the name, as it appears in the combo box, for the Num-th regexp.
   --  The first regexp is number 1.

   function Get_Nth_Search_Regexp
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class; Num : Natural)
      return String;
   --  Return the Num-th regular expression

   function Get_Tab_Width return Natural;
   --  Return the default Tab width.

private

   Open_Options_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;
   Close_Options_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf := Gdk.Pixbuf.Null_Pixbuf;

   type Vsearch_Record is new Gtk_Box_Record with record
      Table               : Gtk_Table;
      Replace_Label       : Gtk_Label;
      Search_For_Label    : Gtk_Label;
      Search_In_Label     : Gtk_Label;
      Replace_Combo       : Gtk_Combo;
      Replace_Entry       : Gtk_Entry;
      Context_Combo       : Gtk_Combo;
      Context_Entry       : Gtk_Entry;
      Pattern_Combo       : Gtk_Combo;
      Pattern_Entry       : Gtk_Entry;
      Buttons_Table       : Gtk_Table;
      Options_Frame       : Gtk_Box;
      Options_Vbox        : Gtk_Table;
      Select_Editor_Check : Gtk_Check_Button;
      Case_Check          : Gtk_Check_Button;
      Whole_Word_Check    : Gtk_Check_Button;
      Auto_Hide_Check     : Gtk_Check_Button;
      Regexp_Check        : Gtk_Check_Button;
      Context_Specific    : Gtk_Box;

      Kernel                 : GPS.Kernel.Kernel_Handle;
      Search_Next_Button     : Gtk.Button.Gtk_Button;
      Replace_Button         : Gtk.Button.Gtk_Button;
      Replace_Search_Button  : Gtk.Button.Gtk_Button;
      Replace_All_Button     : Gtk.Button.Gtk_Button;
      Search_Previous_Button : Gtk.Button.Gtk_Button;
      Search_All_Button      : Gtk.Button.Gtk_Button;
      Replace_Only_Button    : Gtk.Button.Gtk_Button;
      Continue               : Boolean := True;
      Extra_Information      : Gtk.Widget.Gtk_Widget;
      Options_Box            : Collapsing_Pane.Collapsing_Pane;
      Search_Idle_Handler    : Gtk.Main.Idle_Handler_Id := 0;
      Last_Search_Context    : Find_Utils.Search_Context_Access;
      Find_Next              : Boolean := False;
      Found                  : Boolean := False;
      --  Whether the search results in at least one match.
   end record;

   type Search_Regexp is record
      Name           : GNAT.OS_Lib.String_Access;
      Regexp         : GNAT.OS_Lib.String_Access;
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
   end record;

   type Search_Regexps_Array is array (Natural range <>) of Search_Regexp;
   type Search_Regexps_Array_Access is access Search_Regexps_Array;

end Vsearch;
