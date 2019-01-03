------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with GPS.Kernel;
with Gtk.Scrolled_Window;
with Xref;

package Refactoring.UI is

   package Location_Arrays is new Ada.Containers.Doubly_Linked_Lists
     (Xref.General_Location, Xref."=");
   --  Handling of dynamic arrays

   package Source_File_Sets renames GPS.Kernel.File_Sets;
   subtype Source_File_Set is Source_File_Sets.Set;

   function Confirm_Files
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Read_Only_Files : Source_File_Set;
      No_LI_List      : Source_File_Set;
      Stale_LI_List   : Source_File_Set) return Boolean;
   --  Whether the user wants to perform the refactoring even though there are
   --  some errors in the LI files.

   function Dialog
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title         : String;
      Msg           : String;
      Files         : Source_File_Set;
      Execute_Label : String := "Execute";
      Cancel_Label  : String := "Cancel") return Boolean;
   --  Display a dialog to the user, with two buttons: OK and Cancel.
   --  Msg is displayed at the top, and the list of files below.
   --  True is returned if the user pressed OK.
   --  Execute_Label is the label for the OK button

   function Create_File_List
     (List : Source_File_Set)
      return Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   --  Create a list showing all the files in List

end Refactoring.UI;
