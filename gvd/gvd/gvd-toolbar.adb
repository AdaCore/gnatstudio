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

with Gtk.Enums;       use Gtk.Enums;
with Gtk.Pixmap;      use Gtk.Pixmap;
with Odd_Intl;        use Odd_Intl;
with Pixmaps_IDE;     use Pixmaps_IDE;

with Gtkada.Handlers; use Gtkada.Handlers;

with GVD.Process;     use GVD.Process;
with GVD.Types;       use GVD.Types;
with Debugger;        use Debugger;
with Process_Proxies; use Process_Proxies;

package body GVD.Toolbar is

   --------------------
   -- Create_Toolbar --
   --------------------

   function Create_Toolbar
     (Window : access Gtk_Window_Record'Class) return Gtkada_Toolbar
   is
      Toolbar : Gtkada_Toolbar;
      Button  : Gtk_Widget;

   begin
      Gtk_New (Toolbar, Orientation_Horizontal, Toolbar_Icons);
      Set_Tooltips (Toolbar, True);
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
         Tooltip_Text => -"Step one instruction exactly",
         Icon => Gtk_Widget (Create_Pixmap (stepi_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Step_Instruction'Access, Window);
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
         Tooltip_Text =>
           -"Step one instruction, but proceed through subroutine calls",
         Icon => Gtk_Widget (Create_Pixmap (nexti_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Next_Instruction'Access, Window);
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
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Select and print stack frame that called this one",
         Icon => Gtk_Widget (Create_Pixmap (up_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Up'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Select and print stack frame called by this one",
         Icon => Gtk_Widget (Create_Pixmap (down_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Down'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Interrupt debugged program",
         Icon => Gtk_Widget (Create_Pixmap (interrupt_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Interrupt'Access, Window);

      return Toolbar;
   end Create_Toolbar;

   ------------
   -- On_Run --
   ------------

   procedure On_Run
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Run (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Run;

   --------------
   -- On_Start --
   --------------

   procedure On_Start
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Start (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Start;

   -------------
   -- On_Step --
   -------------

   procedure On_Step
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Into (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step;

   -------------------------
   -- On_Step_Instruction --
   -------------------------

   procedure On_Step_Instruction
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Into_Instruction (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step_Instruction;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Over (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next;

   -------------------------
   -- On_Next_Instruction --
   -------------------------

   procedure On_Next_Instruction
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Step_Over_Instruction (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next_Instruction;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Finish (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Finish;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Continue (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Continue;

   -----------
   -- On_Up --
   -----------

   procedure On_Up
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Stack_Up (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Up;

   -------------
   -- On_Down --
   -------------

   procedure On_Down
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null then
         Stack_Down (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Down;

   ------------------
   -- On_Interrupt --
   ------------------

   procedure On_Interrupt
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab = null then
         return;
      end if;

      --  Give some visual feedback to the user
      Output_Text (Tab, "<^C>" & ASCII.LF, Is_Command => True);
      Unregister_Dialog (Tab);

      --  Need to flush the queue of commands
      Clear_Queue (Tab.Debugger);
      Interrupt (Tab.Debugger);

      if not Command_In_Process (Get_Process (Tab.Debugger)) then
         Display_Prompt (Tab.Debugger);
      end if;
   end On_Interrupt;

end GVD.Toolbar;
