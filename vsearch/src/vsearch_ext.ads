-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Find_Utils;
with Glide_Kernel;
with Gtk.Button;
with Gtk.Main;
with Gtk.Toggle_Button;
with Gtk.Widget;
with Vsearch_Pkg;
with GNAT.OS_Lib;

--  This package provides an extended version of the visual search
--  widget that can be found in module vsearch, so that it can be integrated
--  within the project explorer directly.

package Vsearch_Ext is

   type Vsearch_Extended_Record is new Vsearch_Pkg.Vsearch_Record with private;
   type Vsearch_Extended is access all Vsearch_Extended_Record'Class;

   procedure Gtk_New
     (Vsearch : out Vsearch_Extended;
      Handle  : Glide_Kernel.Kernel_Handle);
   --  Create a new extended search dialog.

   procedure Initialize
     (Vsearch : access Vsearch_Extended_Record'Class;
      Handle  : Glide_Kernel.Kernel_Handle);
   --  Internal initialization procedure.

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Register_Search_Function
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Data   : Find_Utils.Search_Module_Data);
   --  See Find_Utils.Register_Search_Function;

   function Search_Context_From_Module
     (Id : access Glide_Kernel.Module_ID_Record'Class)
      return Find_Utils.Search_Module_Data;
   --  See Find_Utils.Context_From_Module;

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
   --  You should use the interface in Glide_Kernel.Custom instead of directly
   --  calling the subprograms below to limit the dependencies on this package

   procedure Register_Search_Pattern
     (Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Regexp         : String;
      Case_Sensitive : Boolean := False;
      Is_Regexp      : Boolean := True);
   --  Register a new template regular expression in the search engine.
   --  Name will appear in the popdown menu of the combo box, but this will be
   --  associated with the regular expression Regexp.
   --  This emits the "search_regexps_changed" signal on Kernel.

   function Search_Regexps_Count
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) return Natural;
   --  Return the number of registered predefined patterns

   procedure Get_Nth_Search_Regexp_Options
     (Kernel         : access Glide_Kernel.Kernel_Handle_Record'Class;
      Num            : Natural;
      Case_Sensitive : out Boolean;
      Is_Regexp      : out Boolean);
   --  Return the options for the Num-th predefined search regexp

   function Get_Nth_Search_Regexp_Name
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class; Num : Natural)
      return String;
   --  Return the name, as it appears in the combo box, for the Num-th regexp.
   --  The first regexp is number 1.

   function Get_Nth_Search_Regexp
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class; Num : Natural)
      return String;
   --  Return the Num-th regular expression


private
   type Vsearch_Extended_Record is new Vsearch_Pkg.Vsearch_Record with record
      Kernel                 : Glide_Kernel.Kernel_Handle;
      Search_Next_Button     : Gtk.Button.Gtk_Button;
      Search_Replace_Button  : Gtk.Button.Gtk_Button;
      Search_Previous_Button : Gtk.Button.Gtk_Button;
      Stop_Button            : Gtk.Button.Gtk_Button;
      Options_Toggle         : Gtk.Toggle_Button.Gtk_Toggle_Button;
      Continue               : Boolean := True;
      Extra_Information      : Gtk.Widget.Gtk_Widget;
      Search_Idle_Handler    : Gtk.Main.Idle_Handler_Id := 0;
      Last_Search_Context    : Find_Utils.Search_Context_Access;
      Find_Next              : Boolean := False;
   end record;

   type Search_Regexp is record
      Name           : GNAT.OS_Lib.String_Access;
      Regexp         : GNAT.OS_Lib.String_Access;
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
   end record;

   type Search_Regexps_Array is array (Natural range <>) of Search_Regexp;
   type Search_Regexps_Array_Access is access Search_Regexps_Array;

end Vsearch_Ext;
