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

--  This package implements the project hierarchy browsers

with Browsers.Canvas;
with Glib;
with Glide_Kernel;
with Gdk.Event;
with Gtk.Menu;
with Types;
with Projects;
with Pango.Layout;

package Browsers.Projects is

   type Browser_Project_Vertex is new
     Browsers.Canvas.Arrow_Item_Record with private;
   type Browser_Project_Vertex_Access is access all Browser_Project_Vertex;

   type Project_Browser_Record is new Browsers.Canvas.General_Browser_Record
     with null record;
   type Project_Browser is access all Project_Browser_Record'Class;

   function Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

private
   procedure Resize_And_Draw
     (Item             : access Browser_Project_Vertex;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   procedure Gtk_New
     (V       : out Browser_Project_Vertex_Access;
      Browser : access Project_Browser_Record'Class;
      Project : Standard.Projects.Project_Type);
   --  Create a new project vertex

   type Browser_Project_Vertex is new Browsers.Canvas.Arrow_Item_Record with
      record
         Name    : Types.Name_Id;
      end record;
end Browsers.Projects;
