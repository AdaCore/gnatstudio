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

with Glide_Kernel.Help; use Glide_Kernel.Help;

package body Find_Utils.Help is

   ------------
   -- Search --
   ------------

   function Search
     (Context         : access Help_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean is
   begin
      if Context.BM_Initialized then
         return Search_Next (Kernel);
      else
         Context.BM_Initialized := True;
         return Search
           (Kernel,
            Context.Look_For.all,
            Context.Options.Case_Sensitive,
            not Search_Backward,
            Context.Options.Regexp);
      end if;
   end Search;

   ------------------
   -- Help_Factory --
   ------------------

   function Help_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      Extra_Information : Gtk.Widget.Gtk_Widget) return Search_Context_Access
   is
      pragma Unreferenced (Kernel, Extra_Information);

      Context : Help_Context_Access;
   begin
      Context := new Help_Context;
      return Search_Context_Access (Context);
   end Help_Factory;

end Find_Utils.Help;
