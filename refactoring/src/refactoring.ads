-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007, AdaCore              --
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

with Entities;
with Dynamic_Arrays;
with GPS.Kernel;
with Gtk.Scrolled_Window;
with Gtk.Stock;

with Basic_Types; use Basic_Types;

package Refactoring is

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
   package File_Arrays is new Dynamic_Arrays
     (Data                    => Entities.Source_File,
      Table_Multiplier        => 2,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 10,
      "="                     => Entities."=");
   --  Handling of dynamic arrays

   function Confirm_Files
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Read_Only_Files : File_Arrays.Instance;
      No_LI_List      : File_Arrays.Instance;
      Stale_LI_List   : File_Arrays.Instance) return Boolean;
   --  Whether the user wants to perform the refactoring even though there are
   --  some errors in the LI files.

   function Dialog
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title  : String;
      Msg    : String;
      Files  : File_Arrays.Instance;
      Execute_Label : String := Gtk.Stock.Stock_Execute;
      Cancel_Label  : String := Gtk.Stock.Stock_Cancel) return Boolean;
   --  Display a dialog to the user, with two buttons: OK and Cancel.
   --  Msg is displayed at the top, and the list of files below.
   --  True is returned if the user pressed OK.
   --  Execute_Label is the label for the OK button

   function Create_File_List
     (List : File_Arrays.Instance)
      return Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   --  Create a list showing all the files in List.

end Refactoring;
