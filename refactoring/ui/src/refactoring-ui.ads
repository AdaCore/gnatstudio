------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;
with Entities;
with Dynamic_Arrays;
with GPS.Kernel;
with Gtk.Scrolled_Window;
with Gtk.Stock;

with Basic_Types; use Basic_Types;

package Refactoring.UI is

   type Location_Type is record
      File   : Entities.Source_File;
      Line   : Integer;
      Column : Visible_Column_Type;
   end record;
   --  A location in a file. This is light-weight compared to E_Reference,
   --  and can be used for declarations as well.

   package Location_Arrays is new Dynamic_Arrays
     (Data                    => Location_Type,
      Table_Multiplier        => 2,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 100);
   --  Handling of dynamic arrays

   function Hash (File : Entities.Source_File) return Ada.Containers.Hash_Type;
   package Source_File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Entities.Source_File,
      Hash                => Hash,
      Equivalent_Elements => Entities."=",
      "="                 => Entities."=");
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
      Execute_Label : String := Gtk.Stock.Stock_Execute;
      Cancel_Label  : String := Gtk.Stock.Stock_Cancel) return Boolean;
   --  Display a dialog to the user, with two buttons: OK and Cancel.
   --  Msg is displayed at the top, and the list of files below.
   --  True is returned if the user pressed OK.
   --  Execute_Label is the label for the OK button

   function Create_File_List
     (List : Source_File_Set)
      return Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   --  Create a list showing all the files in List

end Refactoring.UI;
