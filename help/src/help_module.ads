-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

--  This package provides generic Help facilities.

with Glide_Kernel;
with Find_Utils;
with Gtk.Widget;

package Help_Module is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   --------------------
   -- Search context --
   --------------------

   type Help_Context is new Find_Utils.Search_Context with private;
   type Help_Context_Access is access all Help_Context'Class;
   --  A special context for searching in the help

   function Search
     (Context         : access Help_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Help"

   function Help_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Find_Utils.Search_Context_Access;
   --  Search factory for "Help"

   procedure Show_Tutorial
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Open the help window and display the tutorial

private

   type Help_Context is new Find_Utils.Search_Context with record
      First_Search : Boolean;
      --  true if the next search will be the first for this context
   end record;

end Help_Module;
