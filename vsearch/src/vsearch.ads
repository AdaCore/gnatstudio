------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  This package provides the Search view.

with Glib.Object;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Widget;          use Gtk.Widget;

with Find_Utils;          use Find_Utils;
with GPS.Kernel;          use GPS.Kernel;
with Projects;

package Vsearch is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   ---------------------
   -- Search contexts --
   ---------------------

   procedure Register_Search_Function
     (Kernel     : access Kernel_Handle_Record'Class;
      Module     : not null access Search_Module_Type'Class;
      Is_Default : Boolean := False);
   --  Register a new search function.
   --  This will be available under the title Label in the search combo box.
   --  This procedure immediately emits the kernel signal
   --  "search_functions_changed".
   --
   --  When Is_Default is True, the search function will be used by default
   --  when no module has been found when creating the dialog (e.g: when the
   --  user dit not focus any module-related widget yet).

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

   procedure Get_Selection
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      From   : out Gtk_Text_Mark;
      To     : out Gtk_Text_Mark);
   --  Return selection region saved at the beginning of last search

   function Get_Selected_Project
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Projects.Project_Type_Array;
   --  Return selected project saved at the beginning of last search

end Vsearch;
