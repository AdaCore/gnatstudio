-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
--                             ACT-Europe                            --
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

with Gtk.Enums;               use Gtk.Enums;
with Gtk.Pixmap;              use Gtk.Pixmap;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Handlers;         use Gtkada.Handlers;
with GVD.Toolbar;             use GVD.Toolbar;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Intl;              use Glide_Intl;
with Pixmaps_IDE;             use Pixmaps_IDE;
with Traces;                  use Traces;

package body GVD_Module is

   GVD_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Debugger");

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar   : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Window    : constant Gtk_Window  := Get_Main_Window (Kernel);
      Button    : Gtk_Widget;

   begin
      Append_Space (Toolbar);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Start the debugged program",
         Icon => Gtk_Widget (Create_Pixmap (run_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Run'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text =>
           (-"Start the debugged program, ") &
           (-"stopping at the beginning of the main procedure"),
         Icon => Gtk_Widget (Create_Pixmap (start_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Start'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text =>
           -"Step program until it reaches a different source line",
         Icon => Gtk_Widget (Create_Pixmap (step_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Step'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Step program, proceeding through subroutine calls",
         Icon => Gtk_Widget (Create_Pixmap (next_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Next'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Execute until selected stack frame returns",
         Icon => Gtk_Widget (Create_Pixmap (finish_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Finish'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text =>
           -"Continue program being debugged, after signal or breakpoint",
         Icon => Gtk_Widget (Create_Pixmap (cont_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Continue'Access, Window);
   end Initialize_Module;

begin
   GVD_Module_ID := Register_Module
     (Module_Name             => GVD_Module_Name,
      Priority                => Default_Priority,
      Initializer             => Initialize_Module'Access,
      Contextual_Menu_Handler => null);
end GVD_Module;
