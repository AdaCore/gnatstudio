-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Gtk;             use Gtk;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Pixmap;      use Gtk.Pixmap;
with Gtkada.Handlers; use Gtkada.Handlers;
with Odd_Intl;        use Odd_Intl;
with Main_Debug_Window_Pkg.Callbacks; use Main_Debug_Window_Pkg.Callbacks;
with Pixmaps_IDE;     use Pixmaps_IDE;

package body Main_Debug_Window_Pkg is

procedure Gtk_New (Main_Debug_Window : out Main_Debug_Window_Access) is
begin
   Main_Debug_Window := new Main_Debug_Window_Record;
   Main_Debug_Window_Pkg.Initialize (Main_Debug_Window);
end Gtk_New;

procedure Initialize (Main_Debug_Window : access Main_Debug_Window_Record'Class) is
   pragma Suppress (All_Checks);

begin
   Gtk.Window.Initialize (Main_Debug_Window, Window_Toplevel);
   Set_Title (Main_Debug_Window, -"The GNU Visual Debugger");
   Set_Policy (Main_Debug_Window, False, True, False);
   Set_Position (Main_Debug_Window, Win_Pos_None);
   Set_Modal (Main_Debug_Window, False);
   Set_Default_Size (Main_Debug_Window, 700, 700);
   Return_Callback.Connect
     (Main_Debug_Window, "delete_event", On_Main_Debug_Window_Delete_Event'Access);

   Gtk_New_Vbox (Main_Debug_Window.Vbox, False, 0);
   Add (Main_Debug_Window, Main_Debug_Window.Vbox);

   Gtk_New (Main_Debug_Window.Toolbar, Orientation_Horizontal, Toolbar_Icons);
   Set_Space_Size (Main_Debug_Window.Toolbar, 5);
   Set_Space_Style (Main_Debug_Window.Toolbar, Toolbar_Space_Empty);
   Set_Tooltips (Main_Debug_Window.Toolbar, True);
   Set_Button_Relief (Main_Debug_Window.Toolbar, Relief_Normal);
   Main_Debug_Window.Button49 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Start the debugged program",
      Icon => Gtk_Widget (Create_Pixmap (run_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button49, "clicked", On_Run1_Toolbar_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button50 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Start the debugged program, stopping at the beginning of the main procedure",
      Icon => Gtk_Widget (Create_Pixmap (start_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button50, "clicked", On_Start1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button52 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step program until it reaches a different source line",
      Icon => Gtk_Widget (Create_Pixmap (step_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button52, "clicked", On_Step1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button53 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step one instruction exactly",
      Icon => Gtk_Widget (Create_Pixmap (stepi_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button53, "clicked", On_Step_Instruction1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button54 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step program, proceeding through subroutine calls",
      Icon => Gtk_Widget (Create_Pixmap (next_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button54, "clicked", On_Next1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button55 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Step one instruction, but proceed through subroutine calls",
      Icon => Gtk_Widget (Create_Pixmap (nexti_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button55, "clicked", On_Next_Instruction1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button58 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Execute until selected stack frame returns",
      Icon => Gtk_Widget (Create_Pixmap (finish_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button58, "clicked", On_Finish1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button60 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Continue program being debugged, after signal or breakpoint",
      Icon => Gtk_Widget (Create_Pixmap (cont_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button60, "clicked", On_Continue1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button57 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Select and print stack frame that called this one",
      Icon => Gtk_Widget (Create_Pixmap (up_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button57, "clicked", On_Up1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button51 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Select and print stack frame called by this one",
      Icon => Gtk_Widget (Create_Pixmap (down_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button51, "clicked", On_Down1_Activate'Access, Main_Debug_Window);
   Main_Debug_Window.Button61 := Append_Element
     (Toolbar => Main_Debug_Window.Toolbar,
      The_Type => Toolbar_Child_Button,
      Text => -"",
      Tooltip_Text => -"Interrupt debugged program",
      Icon => Gtk_Widget (Create_Pixmap (interrupt_xpm, Main_Debug_Window)));
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Button61, "clicked", On_Interrupt1_Activate'Access, Main_Debug_Window);
   Pack_Start (Main_Debug_Window.Vbox,
     Get_Handle_Box (Main_Debug_Window.Toolbar), False, False, 0);

   Gtk_New (Main_Debug_Window.Frame);
   Set_Shadow_Type (Main_Debug_Window.Frame, Shadow_Etched_In);
   Pack_Start (Main_Debug_Window.Vbox, Main_Debug_Window.Frame, True, True, 0);

   Gtk_New (Main_Debug_Window.Process_Notebook);
   Set_Scrollable (Main_Debug_Window.Process_Notebook, True);
   Set_Show_Border (Main_Debug_Window.Process_Notebook, True);
   Set_Show_Tabs (Main_Debug_Window.Process_Notebook, False);
   Set_Tab_Hborder (Main_Debug_Window.Process_Notebook, 2);
   Set_Tab_Vborder (Main_Debug_Window.Process_Notebook, 2);
   Set_Tab_Pos (Main_Debug_Window.Process_Notebook, Pos_Top);
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Process_Notebook, "switch_page", On_Process_Notebook_Switch_Page'Access, Main_Debug_Window);
   Add (Main_Debug_Window.Frame, Main_Debug_Window.Process_Notebook);

   Gtk_New (Main_Debug_Window.Statusbar);
   Pack_Start (Main_Debug_Window.Vbox, Main_Debug_Window.Statusbar, False, False, 0);

end Initialize;

end Main_Debug_Window_Pkg;
