-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package provides toolbar creation capabilities for GVD

with Gtkada.Toolbar; use Gtkada.Toolbar;
with Gtk.Widget;     use Gtk.Widget;
with Gtk.Window;     use Gtk.Window;

package GVD.Toolbar is

   function Create_Toolbar
     (Window : access Gtk_Window_Record'Class) return Gtkada_Toolbar;
   --  Create a toolbar containing common debugger buttons.
   --  Window is the GVD top level window.

   procedure On_Run (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "run" button

   procedure On_Start (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "start" button

   procedure On_Step (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "step" button

   procedure On_Step_Instruction (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "step instruction" button

   procedure On_Next (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "next" button

   procedure On_Next_Instruction (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "next instruction" button

   procedure On_Finish (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "finish" button

   procedure On_Continue (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "cont" button

   procedure On_Up (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "up" button

   procedure On_Down (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "down" button

   procedure On_Interrupt (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "interrupt" button

end GVD.Toolbar;
