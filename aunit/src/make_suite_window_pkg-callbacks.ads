------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      Make_Suite_Window_Pkg.Callbacks                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2001 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Make_Suite_Window_Pkg.Callbacks is
   --  Handle callbacks for main "AUnit_Make_Suite" window.  Generated
   --  by Glade

   function On_Make_Suite_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Add_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Remove_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class);

end Make_Suite_Window_Pkg.Callbacks;
