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

--  This package implements the project hierarchy browsers

with Browsers.Canvas;
with Glide_Kernel;
with Prj.Tree;
with Gdk.Event;
with Gtk.Menu;
with Types;

package Browsers.Projects is

   type Browser_Project_Vertex is new Browsers.Canvas.Glide_Browser_Item_Record
     with private;
   type Browser_Project_Vertex_Access is access all Browser_Project_Vertex;

   type Project_Browser_Record is new Browsers.Canvas.Glide_Browser_Record with
     null record;
   type Project_Browser is access all Project_Browser_Record'Class;

   procedure Examine_Project_Hierarchy
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      In_Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Project    : Prj.Tree.Project_Node_Id);
   --  Display the project hierarchy for Project in the canvas.

   function Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   procedure Refresh
     (Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Item    : access Browser_Project_Vertex);
   --  Redraw the item to its double buffer

   procedure Register_Module;
   --  Register the module in the list

private
   type Browser_Project_Vertex is new Browsers.Canvas.Glide_Browser_Item_Record
   with record
      Name    : Types.Name_Id;
      Browser : Browsers.Canvas.Glide_Browser;
   end record;

   procedure On_Button_Click
     (Item   : access Browser_Project_Vertex;
      Event  : Gdk.Event.Gdk_Event_Button);
   --  Callback for when the item is clicked

end Browsers.Projects;
