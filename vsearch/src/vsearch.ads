------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with GNAT.Strings;

with Glib.Main;
with Glib.Object;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo_Box;       use Gtk.Combo_Box;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Table;           use Gtk.Table;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Viewport;        use Gtk.Viewport;
with Gtk.Widget;          use Gtk.Widget;

with Find_Utils;          use Find_Utils;
with GPS.Kernel;          use GPS.Kernel;
with Projects;

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

   procedure Register_Preferences (Kernel : access Kernel_Handle_Record'Class);
   --  Register the preferences associated to the search functions

   ---------------------
   -- Search contexts --
   ---------------------

   type Search_Options_Mask is mod 256;
   Case_Sensitive    : constant Search_Options_Mask := 2 ** 1;
   Whole_Word        : constant Search_Options_Mask := 2 ** 2;
   --  Regexp            : constant Search_Options_Mask := 2 ** 3;
   All_Occurrences   : constant Search_Options_Mask := 2 ** 4;
   Search_Backward   : constant Search_Options_Mask := 2 ** 5;
   Supports_Replace  : constant Search_Options_Mask := 2 ** 6;
   All_Options       : constant Search_Options_Mask := 255;
   --  Which options are supported by which contextual search

   procedure Register_Search_Function
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Label             : String;
      Factory           : Find_Utils.Module_Search_Context_Factory;
      Extra_Information : access Gtk.Widget.Gtk_Widget_Record'Class := null;
      Id           : access GPS.Kernel.Abstract_Module_ID_Record'Class := null;
      Mask         : Search_Options_Mask := All_Options;
      In_Selection : Boolean := False);
   --  Register a new search function.
   --  This will be available under the title Label in the search combo box.
   --  This procedure immediately emits the kernel signal
   --  "search_functions_changed".
   --
   --  The Label an include %p for the current project (as understood by the
   --  search dialog).
   --
   --  If Extra_Information is not null, then it will be displayed every time
   --  this label is selected. It can be used for instance to ask for more
   --  information like a list of files to search.
   --  Whenever the data in Extra_Information changes, or for some reason the
   --  current status of GPS no longer permits the search, you should raise the
   --  kernel signal Search_Reset_Signal (or call Reset_Search below).
   --
   --  When the user then selects "Find", the function Factory is called to
   --  create the factory. The options and searched string or regexp will be
   --  set automatically on return of Factory, so you do not need to handle
   --  this.
   --
   --  Mask indicates what options are relevant for that module. Options that
   --  are not set will be greyed out. If Supports_Replace if false, then the
   --  button will be greyed out.
   --
   --  Id can be left null. If not null, it will be used to set the default
   --  search context when the search dialog is popped up (the first
   --  search_module_data that matches the current module is used).

   --  When Id is not null and In_Selection = True it will be used to set the
   --  default search context when there multiline selection in an editor.

   procedure Reset_Search
     (Object : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Raises the kernel signal Search_Reset_Signal. This is just a convenience
   --  callback function. Object is ignored, and can be anything.

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

   procedure Get_Selection
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      From   : out Gtk_Text_Mark;
      To     : out Gtk_Text_Mark);
   --  Return selection region saved at the beginning of last search

   function Get_Selected_Project
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Projects.Project_Type_Array;
   --  Return selected project saved at the beginning of last search

private

   type Vsearch_Record is new Gtk_Box_Record with record
      Scrolled                : Gtk_Scrolled_Window;
      View                    : Gtk_Viewport;
      Main                    : Gtk_Vbox;
      Table                   : Gtk_Table;
      Replace_Label           : Gtk_Label;
      Search_For_Label        : Gtk_Label;
      Search_In_Label         : Gtk_Label;
      Replace_Combo           : Gtk_Combo_Box;
      Context_Combo           : Gtk_Combo_Box_Text;
      Pattern_Combo           : Gtk_Combo_Box;
      Buttons_Table           : Gtk_Table;
      Options_Frame           : Gtk_Box;
      Options_Vbox            : Gtk_Table;
      Scope_Frame             : Gtk_Box;
      Scope_Table             : Gtk_Table;
      Select_Editor_Check     : Gtk_Check_Button;
      Case_Check              : Gtk_Check_Button;
      Case_Preserving_Replace : Gtk_Check_Button;
      Whole_Word_Check        : Gtk_Check_Button;
      Auto_Hide_Check         : Gtk_Check_Button;
      Regexp_Check            : Gtk_Check_Button;
      Context_Specific        : Gtk_Box;

      Kernel                  : GPS.Kernel.Kernel_Handle;
      Search_Next_Button      : Gtk.Button.Gtk_Button;
      Replace_Button          : Gtk.Button.Gtk_Button;
      Replace_Search_Button   : Gtk.Button.Gtk_Button;
      Replace_All_Button      : Gtk.Button.Gtk_Button;
      Search_Previous_Button  : Gtk.Button.Gtk_Button;
      Search_All_Button       : Gtk.Button.Gtk_Button;
      Replace_Only_Button     : Gtk.Button.Gtk_Button;
      Extra_Information       : Gtk.Widget.Gtk_Widget;
      Search_Idle_Handler     : Glib.Main.G_Source_Id := 0;
      Last_Search_Context     : Find_Utils.Root_Search_Context_Access;
      --  This is the context used for single Find/Next and Replace operations.
      Last_Search_All_Context : Find_Utils.Root_Search_Context_Access;
      --  This is the context used for Find/Replace All operations. It is
      --  then copied to the idle data of the background command. The purpose
      --  here it to be able to launch a new Find/Replace All operation while
      --  there is another one already running: they need to have their own
      --  context.
      Find_Next               : Boolean := False;
      Selection_From          : Gtk_Text_Mark;
      Selection_To            : Gtk_Text_Mark;

      Projects                : Standard.Projects.Project_Type_Array_Access;
      --  Restrict the search to these projects
   end record;

   type Search_Regexp is record
      Name           : GNAT.Strings.String_Access;
      Regexp         : GNAT.Strings.String_Access;
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
   end record;

   type Search_Regexps_Array is array (Natural range <>) of Search_Regexp;
   type Search_Regexps_Array_Access is access Search_Regexps_Array;

end Vsearch;
