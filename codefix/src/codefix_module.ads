-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package defines the module for code fixing.

with Gtk.Menu;               use Gtk.Menu;
with Glide_Kernel;           use Glide_Kernel;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with Codefix;                use Codefix;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Text_Manager;

package Codefix_Module is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Codefix_Session_Record is record
      Category     : GNAT.OS_Lib.String_Access;
      Corrector    : Ptr_Correction_Manager;
      Current_Text : Codefix.Text_Manager.Ptr_Text_Navigator;
   end record;
   type Codefix_Session is access all Codefix_Session_Record;

   function Create_Submenu
     (Kernel       : access Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id) return Gtk_Menu;
   --  Return a menu with all the possible fixes for Error

   procedure Create_Pixmap_And_Category
     (Kernel       : access Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id);
   --  Add to the location box a pixmap that will fixes the error.

   procedure Remove_Pixmap
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Session      : access Codefix_Session_Record;
      Error        : Error_Id);
   --  Remove from the location box the pixmap of the error.

end Codefix_Module;
