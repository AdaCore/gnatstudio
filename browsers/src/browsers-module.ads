-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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

with Browsers.Canvas;
with Glide_Kernel;
with Gtkada.MDI;
with Gtk.Widget;

package Browsers.Module is

   function Open_Browser
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Browser_Type : Browsers.Browser_Type_Mask := Browsers.Any_Browser)
      return Gtkada.MDI.MDI_Child;
   --  Open a new browser that supports all the types described in
   --  Browser_Type.
   --  If there is already a browser in Glide2 that handles all the types
   --  Browser_Type, we re-use this one instead.

   procedure Examine_Dependencies
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      In_Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      File       : String);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.

   procedure Open_File
     (Browser : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Open the file described in Context for analysis in the browser.

end Browsers.Module;
