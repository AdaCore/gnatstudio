-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This package handles the commands necessary to configure/add/remove
--  line highlighting in the source editors.

with GNAT.OS_Lib;
with Gdk.GC; use Gdk.GC;

package Src_Editor_Module.Line_Highlighting is

   function Edit_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Interactive command handler for the source editor module.

   --  The following functions are used to provide a cache-like capability
   --  for storing and retrieving categories.
   --  The category indexes begin at 1.

   procedure Add_Category
     (Id : String;
      GC : Gdk_GC);
   --  Add a new category to the category cache.

   function Lookup_Category (Id : String) return Natural;
   --  Return the index corresponding to Id.
   --  Return 0 if the category wasn't found.

   function Get_GC (Index : Natural) return Gdk_GC;
   --  Return the GC corresponding to a category Index.

   function Get_Last_Index return Natural;
   --  Return the number of categories.

end Src_Editor_Module.Line_Highlighting;
