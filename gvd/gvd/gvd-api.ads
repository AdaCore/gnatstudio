-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
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

--  This package contains all the public functions that can be used
--  outside GVD, e.g when using GVD as a library, or as an object (Corba,
--  COM, Bonobo, ...)
--
--  The following API is compatible with C, so that it can be used in as many
--  different ways as possible.
--
--  It is not intended to be used from Ada directly.

with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package GVD.API is

   type Main_Debug_Window is private;
   --  typedef struct _main_debug_window *main_debug_window;

   type Debugger_Process_Tab is private;
   --  typedef struct _debugger_process_tab *debugger_process_tab;

   type Code_Editor is private;
   --  typedef struct _code_editor *code_editor;

   type Source_Editor is private;
   --  typedef struct _source_editor *source_editor;

   type Debugger_Type is
     (Gdb_Type, Dbx_Type, Xdb_Type, Jdb_Type,
      Pydb_Type, Perl_Type, Ladebug_Type);
   for Debugger_Type'Size use Integer'Size;
   --  typedef enum {gdb_type, dbx_type, xdb_type, jdb_type, pydb_type,
   --    perl_type, ladebug_type} debugger_type;

   type Visible_Command is (Visible, User);
   for Visible_Command'Size use Integer'Size;
   --  typedef enum {visible, user} visible_command;

   type View_Mode is (Source_Only, Asm_Only, Source_Asm);
   for View_Mode'Size use Integer'Size;
   --  typedef enum {source_only, asm_only, source_asm} view_mode;

   -----------------
   -- GVD.Process --
   -----------------

   function Create_Debugger
     (Window          : Main_Debug_Window;
      Kind            : Debugger_Type;
      Executable      : chars_ptr;
      Params          : System.Address;
      N_Params        : Integer;
      Remote_Host     : chars_ptr;
      Remote_Target   : chars_ptr;
      Remote_Protocol : chars_ptr;
      Debugger_Name   : chars_ptr) return Debugger_Process_Tab;
   --  debugger_process_tab gvd_create_debugger
   --    (main_debug_window window,
   --     debugger_type     kind,
   --     char *            executable,
   --     char **           params,
   --     int               n_params,
   --     char *            remote_host,
   --     char *            remote_target,
   --     char *            remote_protocol,
   --     char *            debugger_name);

   function Process_Tab_Get_Widget
     (Window : Debugger_Process_Tab) return System.Address;
   --  GtkWidget * gvd_process_tab_get_widget (debugger_process_tab window);

   function Get_Current_Process
     (Main_Window : Main_Debug_Window) return Debugger_Process_Tab;
   --  debugger_process_tab gvd_get_current_process (main_debug_window window);

   procedure Close_Debugger (Debugger : Debugger_Process_Tab);
   --  void gvd_close_debugger (debugger_process_tab debugger);

   procedure Process_User_Command
     (Debugger       : Debugger_Process_Tab;
      Command        : chars_ptr;
      Output_Command : Integer;
      Mode           : Visible_Command);
   --  void gvd_process_user_command
   --    (debugger_process_tab debugger,
   --     char *               command,
   --     int                  output_command,
   --     visible_command      mode);

   ---------------------------
   -- Main_Debug_Window_Pkg --
   ---------------------------

   function Create_Main_Window return Main_Debug_Window;
   --  main_debug_window gvd_create_main_window ();

   function Debug_Window_Get_Widget
     (Window : Main_Debug_Window) return System.Address;
   --  GtkWidget * gvd_debug_window_get_widget (main_debug_window window);

   -------------------------------------
   -- Main_Debug_Window_Pkg.Callbacks --
   -------------------------------------

   procedure Open_Program (Window : Main_Debug_Window);
   --  void gvd_open_program (main_debug_window window);

   procedure Open_Debugger (Window : Main_Debug_Window);
   --  void gvd_open_debugger (main_debug_window window);

   procedure Open_Core_Dump (Window : Main_Debug_Window);
   --  void gvd_open_core_dump (main_debug_window window);

   procedure Edit_Source (Window : Main_Debug_Window);
   --  void gvd_edit_source (main_debug_window window);

   procedure Reload_Source (Window : Main_Debug_Window);
   --  void gvd_reload_source (main_debug_window window);

   procedure Open_Session (Window : Main_Debug_Window);
   --  void gvd_open_session (main_debug_window window);

   procedure Save_Session_As (Window : Main_Debug_Window);
   --  void gvd_save_session_as (main_debug_window window);

   procedure Attach_To_Process (Window : Main_Debug_Window);
   --  void gvd_attach_to_process (main_debug_window window);

   procedure Detach_Process (Window : Main_Debug_Window);
   --  void gvd_detach_process (main_debug_window window);

   procedure Change_Directory (Window : Main_Debug_Window);
   --  void gvd_change_directory (main_debug_window window);

   procedure Close (Window : Main_Debug_Window);
   --  void gvd_close (main_debug_window window);

   procedure GVD_Exit (Window : Main_Debug_Window);
   --  void gvd_exit (main_debug_window window);

   procedure Undo (Window : Main_Debug_Window);
   --  void gvd_undo (main_debug_window window);

   procedure Redo (Window : Main_Debug_Window);
   --  void gvd_redo (main_debug_window window);

   procedure Cut (Window : Main_Debug_Window);
   --  void gvd_cut (main_debug_window window);

   procedure Copy (Window : Main_Debug_Window);
   --  void gvd_copy (main_debug_window window);

   procedure Paste (Window : Main_Debug_Window);
   --  void gvd_paste (main_debug_window window);

   procedure Select_All (Window : Main_Debug_Window);
   --  void gvd_select_all (main_debug_window window);

   procedure Search (Window : Main_Debug_Window);
   --  void gvd_search (main_debug_window window);

   procedure Preferences (Window : Main_Debug_Window);
   --  void gvd_preferences (main_debug_window window);

   procedure Gdb_Settings (Window : Main_Debug_Window);
   --  void gvd_gdb_settings (main_debug_window window);

   procedure Run (Window : Main_Debug_Window);
   --  void gvd_run (main_debug_window window);

   procedure Step (Window : Main_Debug_Window);
   --  void gvd_step (main_debug_window window);

   procedure Step_Instruction (Window : Main_Debug_Window);
   --  void gvd_step_instruction (main_debug_window window);

   procedure Next (Window : Main_Debug_Window);
   --  void gvd_next (main_debug_window window);

   procedure Next_Instruction (Window : Main_Debug_Window);
   --  void gvd_next_instruction (main_debug_window window);

   procedure Finish (Window : Main_Debug_Window);
   --  void gvd_finish (main_debug_window window);

   procedure Continue (Window : Main_Debug_Window);
   --  void gvd_continue (main_debug_window window);

   procedure Continue_Without_Signal (Window : Main_Debug_Window);
   --  void gvd_continue_without_signal (main_debug_window window);

   procedure Kill (Window : Main_Debug_Window);
   --  void gvd_kill (main_debug_window window);

   procedure Interrupt (Window : Main_Debug_Window);
   --  void gvd_interrupt (main_debug_window window);

   procedure GVD_Abort (Window : Main_Debug_Window);
   --  void gvd_abort (main_debug_window window);

   procedure Command_History (Window : Main_Debug_Window);
   --  void gvd_command_history (main_debug_window window);

   procedure Clear_Window (Window : Main_Debug_Window);
   --  void gvd_clear_window (main_debug_window window);

   procedure Define_Command (Window : Main_Debug_Window);
   --  void gvd_define_command (main_debug_window window);

   procedure Edit_Buttons (Window : Main_Debug_Window);
   --  void gvd_edit_buttons (main_debug_window window);

   procedure Call_Stack (Window : Main_Debug_Window);
   --  void gvd_call_stack (main_debug_window window);

   procedure Threads (Window : Main_Debug_Window);
   --  void gvd_threads (main_debug_window window);

   procedure Processes (Window : Main_Debug_Window);
   --  void gvd_processes (main_debug_window window);

   procedure Signals (Window : Main_Debug_Window);
   --  void gvd_signals (main_debug_window window);

   procedure Edit_Breakpoints (Window : Main_Debug_Window);
   --  void gvd_edit_breakpoints (main_debug_window window);

   procedure Edit_Displays (Window : Main_Debug_Window);
   --  void gvd_edit_displays (main_debug_window window);

   procedure Examine_Memory (Window : Main_Debug_Window);
   --  void gvd_examine_memory (main_debug_window window);

   procedure Display_Local_Variables (Window : Main_Debug_Window);
   --  void gvd_display_local_variables (main_debug_window window);

   procedure Display_Arguments (Window : Main_Debug_Window);
   --  void gvd_display_arguments (main_debug_window window);

   procedure Display_Registers (Window : Main_Debug_Window);
   --  void gvd_display_registers (main_debug_window window);

   procedure Display_Expression (Window : Main_Debug_Window);
   --  void gvd_display_expression (main_debug_window window);

   procedure More_Status_Display (Window : Main_Debug_Window);
   --  void gvd_more_status_display (main_debug_window window);

   procedure Refresh (Window : Main_Debug_Window);
   --  void gvd_refresh (main_debug_window window);

   procedure Overview (Window : Main_Debug_Window);
   --  void gvd_overview (main_debug_window window);

   procedure On_Item (Window : Main_Debug_Window);
   --  void gvd_on_item (main_debug_window window);

   procedure What_Now (Window : Main_Debug_Window);
   --  void gvd_what_now (main_debug_window window);

   procedure Tip_Of_The_Day (Window : Main_Debug_Window);
   --  void gvd_tip_of_the_day (main_debug_window window);

   procedure About_GVD (Window : Main_Debug_Window);
   --  void gvd_about_gvd (main_debug_window window);

   procedure Run_Toolbar (Window : Main_Debug_Window);
   --  void gvd_run_toolbar (main_debug_window window);

   procedure Start (Window : Main_Debug_Window);
   --  void gvd_start (main_debug_window window);

   procedure Up (Window : Main_Debug_Window);
   --  void gvd_up (main_debug_window window);

   procedure Down (Window : Main_Debug_Window);
   --  void gvd_down (main_debug_window window);

   ------------------------
   -- GVD.Source_Editors --
   ------------------------

   procedure Set_Breakpoint
     (Process : Debugger_Process_Tab;
      File    : chars_ptr;
      Line    : Integer);
   --  void gvd_set_breakpoint
   --    (debugger_process_tab process, char * file, int line);

   procedure Till_Breakpoint
     (Process : Debugger_Process_Tab;
      File    : chars_ptr;
      Line    : Integer);
   --  void gvd_till_breakpoint
   --    (debugger_process_tab process, char * file, int line);

   procedure Show_Current_Line_Menu (Editor : Source_Editor);
   --  void gvd_show_current_line_menu (source_editor editor);

   procedure Change_Line_Nums (Editor : Source_Editor; Toggle : Integer);
   --  void gvd_change_line_nums (source_editor editor, int toggle);

   procedure Change_Lines_With_Code (Editor : Source_Editor; Toggle : Integer);
   --  void gvd_change_lines_with_code (source_editor editor, int toggle);

   ----------------------
   -- GVD.Code_Editors --
   ----------------------

   procedure Change_Mode (Editor : Code_Editor; Mode : View_Mode);
   --  void gvd_change_mode (code_editor editor, view_mode mode);

private
   type Main_Debug_Window is new System.Address;
   type Debugger_Process_Tab is new System.Address;
   type Code_Editor is new System.Address;
   type Source_Editor is new System.Address;

   pragma Export (C, Create_Debugger, "gvd_create_debugger");
   pragma Export (C, Process_Tab_Get_Widget, "gvd_process_tab_get_widget");
   pragma Export (C, Get_Current_Process, "gvd_get_current_process");
   pragma Export (C, Close_Debugger, "gvd_close_debugger");
   pragma Export (C, Process_User_Command, "gvd_process_user_command");
   pragma Export (C, Create_Main_Window, "gvd_create_main_window");
   pragma Export (C, Debug_Window_Get_Widget, "gvd_debug_window_get_widget");
   pragma Export (C, Open_Program, "gvd_open_program");
   pragma Export (C, Open_Debugger, "gvd_open_debugger");
   pragma Export (C, Open_Core_Dump, "gvd_open_core_dump");
   pragma Export (C, Edit_Source, "gvd_edit_source");
   pragma Export (C, Reload_Source, "gvd_reload_source");
   pragma Export (C, Open_Session, "gvd_open_session");
   pragma Export (C, Save_Session_As, "gvd_save_session_as");
   pragma Export (C, Attach_To_Process, "gvd_attach_to_process");
   pragma Export (C, Detach_Process, "gvd_detach_process");
   pragma Export (C, Change_Directory, "gvd_change_directory");
   pragma Export (C, Close, "gvd_close");
   pragma Export (C, GVD_Exit, "gvd_exit");
   pragma Export (C, Undo, "gvd_undo");
   pragma Export (C, Redo, "gvd_redo");
   pragma Export (C, Cut, "gvd_cut");
   pragma Export (C, Copy, "gvd_copy");
   pragma Export (C, Paste, "gvd_paste");
   pragma Export (C, Select_All, "gvd_select_all");
   pragma Export (C, Search, "gvd_search");
   pragma Export (C, Preferences, "gvd_preferences");
   pragma Export (C, Gdb_Settings, "gvd_settings");
   pragma Export (C, Run, "gvd_run");
   pragma Export (C, Step, "gvd_step");
   pragma Export (C, Step_Instruction, "gvd_step_instruction");
   pragma Export (C, Next, "gvd_next");
   pragma Export (C, Next_Instruction, "gvd_next_instruction");
   pragma Export (C, Finish, "gvd_finish");
   pragma Export (C, Continue, "gvd_continue");
   pragma Export (C, Continue_Without_Signal, "gvd_continue_without_signal");
   pragma Export (C, Kill, "gvd_kill");
   pragma Export (C, Interrupt, "gvd_interrupt");
   pragma Export (C, GVD_Abort, "gvd_abort");
   pragma Export (C, Command_History, "gvd_command_history");
   pragma Export (C, Clear_Window, "gvd_clear_window");
   pragma Export (C, Define_Command, "gvd_define_command");
   pragma Export (C, Edit_Buttons, "gvd_edit_buttons");
   pragma Export (C, Call_Stack, "gvd_call_stack");
   pragma Export (C, Threads, "gvd_threads");
   pragma Export (C, Processes, "gvd_processes");
   pragma Export (C, Signals, "gvd_signals");
   pragma Export (C, Edit_Breakpoints, "gvd_edit_breakpoints");
   pragma Export (C, Edit_Displays, "gvd_edit_displays");
   pragma Export (C, Examine_Memory, "gvd_examine_memory");
   pragma Export (C, Display_Local_Variables, "gvd_display_local_variables");
   pragma Export (C, Display_Arguments, "gvd_display_arguments");
   pragma Export (C, Display_Registers, "gvd_display_registers");
   pragma Export (C, Display_Expression, "gvd_display_expression");
   pragma Export (C, More_Status_Display, "gvd_more_status_display");
   pragma Export (C, Refresh, "gvd_refresh");
   pragma Export (C, Overview, "gvd_overview");
   pragma Export (C, On_Item, "gvd_on_item");
   pragma Export (C, What_Now, "gvd_what_now");
   pragma Export (C, Tip_Of_The_Day, "gvd_tip_of_the_day");
   pragma Export (C, About_GVD, "gvd_about_gvd");
   pragma Export (C, Run_Toolbar, "gvd_run_toolbar");
   pragma Export (C, Start, "gvd_start");
   pragma Export (C, Up, "gvd_up");
   pragma Export (C, Down, "gvd_down");
   pragma Export (C, Set_Breakpoint, "gvd_set_breakpoint");
   pragma Export (C, Till_Breakpoint, "gvd_till_breakpoint");
   pragma Export (C, Show_Current_Line_Menu, "gvd_show_current_line_menu");
   pragma Export (C, Change_Line_Nums, "gvd_change_line_nums");
   pragma Export (C, Change_Lines_With_Code, "gvd_change_lines_with_code");
   pragma Export (C, Change_Mode, "gvd_change_mode");
end GVD.API;
