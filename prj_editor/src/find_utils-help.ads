-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a search context for the Help module.
--  See package Find_Utils for more information about the search contexts.

package Find_Utils.Help is

   -------------------
   -- Help context --
   -------------------

   type Help_Context is new Search_Context with private;
   type Help_Context_Access is access all Help_Context'Class;
   --  A special context for searching in the help

   function Search
     (Context         : access Help_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Help"

   function Help_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Search factory for "Help"

private

   type Help_Context is new Search_Context with null record;

end Find_Utils.Help;
