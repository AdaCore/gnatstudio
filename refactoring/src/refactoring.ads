-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Ada.Unchecked_Deallocation;
with Basic_Types;
with Glide_Kernel;
with Gtk.Scrolled_Window;
with VFS;

package Refactoring is

   type Location_Type is record
      File   : VFS.Virtual_File;
      Line   : Integer;
      Column : Integer;
   end record;
   --  A location in a file. This is light-weight compared to E_Reference,
   --  and can be used for declarations as well.

   type Location_Array is array (Natural range <>) of Location_Type;
   type Location_Array_Access is access Location_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Location_Array, Location_Array_Access);

   procedure Add is new Basic_Types.Add_And_Grow
     (Location_Type, Natural, Location_Array, Location_Array_Access);
   procedure Add is new Basic_Types.Add_And_Grow
     (VFS.Virtual_File, Natural, VFS.File_Array, VFS.File_Array_Access);
   --  Handling of dynamic arrays

   function Confirm_Files
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      No_LI_List    : VFS.File_Array;
      Stale_LI_List : VFS.File_Array) return Boolean;
   --  Whether the user wants to perform the refactoring even though there are
   --  some errors in the LI files.

   function Create_File_List
     (List : VFS.File_Array) return Gtk.Scrolled_Window.Gtk_Scrolled_Window;
   --  Create a list showing all the files in List.

end Refactoring;
