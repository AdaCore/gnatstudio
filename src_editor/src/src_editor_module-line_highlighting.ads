-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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

with Gdk.GC; use Gdk.GC;
with GNAT.Scripts;

package Src_Editor_Module.Line_Highlighting is

   procedure Edit_Command_Handler
     (Data    : in out GNAT.Scripts.Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the source editor module.

   --  The following functions are used to provide a cache-like capability
   --  for storing and retrieving categories.
   --  The category indexes begin at 1.

   procedure Add_Category
     (Style            : Style_Access;
      Mark_In_Speedbar : Boolean := False);
   --  Add a new category to the category cache.
   --  Update the Color of the category if it already exists.
   --  If Mark_In_Speedbar is true, then a mark is added in the speedbar even
   --  if we only highlight part of a line.

   function Lookup_Category (Style : Style_Access) return Natural;
   --  Return the index corresponding to Style.
   --  If there is no category corresponding to Style, create one.

   function Get_GC (Index : Natural) return Gdk_GC;
   --  Return the GC corresponding to a category Index.
   --  If Index does not correspond to an existing category, return null.

   function Get_Last_Index return Natural;
   --  Return the number of categories.

end Src_Editor_Module.Line_Highlighting;
