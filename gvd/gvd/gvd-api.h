/*
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
*/

/*  This package contains all the public functions that can be used
--  outside GVD, e.g when using GVD as a library, or as an object (Corba,
--  COM, Bonobo, ...)
--
--  The following API is compatible with C, so that it can be used in as many
--  different ways as possible.
--
--  It is not intended to be used from Ada directly.
--
--  See gvd-api.ads for a complete spec.
*/

#ifndef _GVD_API
#define _GVD_API
#include <gtk/gtk.h>

   typedef struct _main_debug_window *main_debug_window;

   typedef struct _debugger_process_tab *debugger_process_tab;

   typedef struct _code_editor *code_editor;

   typedef struct _source_editor *source_editor;

   typedef enum {gdb_type, dbx_type, xdb_type, jdb_type, pydb_type,
     perl_type, ladebug_type} debugger_type;

   typedef enum {visible, user} visible_command;

   typedef enum {source_only, asm_only, source_asm} view_mode;

   debugger_process_tab gvd_create_debugger
     (main_debug_window window,
      debugger_type     kind,
      char *            executable,
      char **           params,
      int               n_params,
      char *            remote_host,
      char *            remote_target,
      char *            remote_protocol,
      char *            debugger_name);

   GtkWidget * gvd_process_tab_get_widget (debugger_process_tab window);

   debugger_process_tab gvd_get_current_process (main_debug_window window);

   void gvd_close_debugger (debugger_process_tab debugger);

   void gvd_process_user_command
     (debugger_process_tab debugger,
      char *               command,
      int                  output_command,
      visible_command      mode);

   main_debug_window gvd_create_main_window ();

   GtkWidget * gvd_debug_window_get_widget (main_debug_window window);

   void gvd_open_program (main_debug_window window);

   void gvd_open_debugger (main_debug_window window);

   void gvd_open_core_dump (main_debug_window window);

   void gvd_edit_source (main_debug_window window);

   void gvd_open_session (main_debug_window window);

   void gvd_save_session_as (main_debug_window window);

   void gvd_attach_to_process (main_debug_window window);

   void gvd_detach_process (main_debug_window window);

   void gvd_change_directory (main_debug_window window);

   void gvd_close (main_debug_window window);

   void gvd_exit (main_debug_window window);

   void gvd_undo (main_debug_window window);

   void gvd_redo (main_debug_window window);

   void gvd_cut (main_debug_window window);

   void gvd_copy (main_debug_window window);

   void gvd_paste (main_debug_window window);

   void gvd_select_all (main_debug_window window);

   void gvd_search (main_debug_window window);

   void gvd_preferences (main_debug_window window);

   void gvd_gdb_settings (main_debug_window window);

   void gvd_run (main_debug_window window);

   void gvd_step (main_debug_window window);

   void gvd_step_instruction (main_debug_window window);

   void gvd_next (main_debug_window window);

   void gvd_next_instruction (main_debug_window window);

   void gvd_finish (main_debug_window window);

   void gvd_continue (main_debug_window window);

   void gvd_continue_without_signal (main_debug_window window);

   void gvd_kill (main_debug_window window);

   void gvd_interrupt (main_debug_window window);

   void gvd_abort (main_debug_window window);

   void gvd_command_history (main_debug_window window);

   void gvd_clear_window (main_debug_window window);

   void gvd_define_command (main_debug_window window);

   void gvd_edit_buttons (main_debug_window window);

   void gvd_call_stack (main_debug_window window);

   void gvd_threads (main_debug_window window);

   void gvd_processes (main_debug_window window);

   void gvd_signals (main_debug_window window);

   void gvd_edit_breakpoints (main_debug_window window);

   void gvd_edit_displays (main_debug_window window);

   void gvd_examine_memory (main_debug_window window);

   void gvd_display_local_variables (main_debug_window window);

   void gvd_display_arguments (main_debug_window window);

   void gvd_display_registers (main_debug_window window);

   void gvd_display_expression (main_debug_window window);

   void gvd_more_status_display (main_debug_window window);

   void gvd_refresh (main_debug_window window);

   void gvd_overview (main_debug_window window);

   void gvd_on_item (main_debug_window window);

   void gvd_what_now (main_debug_window window);

   void gvd_tip_of_the_day (main_debug_window window);

   void gvd_about_gvd (main_debug_window window);

   void gvd_run_toolbar (main_debug_window window);

   void gvd_start (main_debug_window window);

   void gvd_up (main_debug_window window);

   void gvd_down (main_debug_window window);

   void gvd_set_breakpoint
     (debugger_process_tab process, char * file, int line);

   void gvd_till_breakpoint
     (debugger_process_tab process, char * file, int line);

   void gvd_show_current_line_menu (source_editor editor);

   void gvd_change_line_nums (source_editor editor, int toggle);

   void gvd_change_lines_with_code (source_editor editor, int toggle);

   void gvd_change_mode (code_editor editor, view_mode mode);

#endif
