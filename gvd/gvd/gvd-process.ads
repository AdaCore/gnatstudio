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

with Glib;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Expect; use GNAT.Expect;

with Gdk.Color;
with Gdk.Font;
with Gdk.Input;
with Gdk.Types;
with Gtk.Object; use Gtk.Object;
with Gtk.Dialog;
with Gtk.Handlers;
with Gtk.Window;
with Gtk.Widget;
with GVD.Canvas;
with Gtkada.Canvas;

with Debugger; use Debugger;
with Main_Debug_Window_Pkg;
with Process_Tab_Pkg;
with Items;
with GVD.Code_Editors;
with GVD.Types;
with GVD.Types;

package GVD.Process is

   package Canvas_Handler is new Gtk.Handlers.Callback
     (GVD.Canvas.GVD_Canvas_Record);

   package Standard_Input_Package is new Gdk.Input.Input_Add
     (Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class);
   --  This package is needed to handle the tty mode.

   procedure Input_Available
     (Debugger  : Standard_Input_Package.Data_Access;
      Source    : Glib.Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  This procedure should be used in conjunction with Standard_Input above.
   --  This is the callback input function that will retrieve the current
   --  page in the process notebook contained by Debugger and send the
   --  command (line read from Source) using Process_User_Command.
   --  Note that this handler currently assumes that Source is the standard
   --  input file descriptor.

   Command_History_Collapse_Entries : constant Boolean := True;
   --  Whether we should collapse entries in the history list.

   ----------------------
   -- Backtrace window --
   ----------------------

   type Stack_List_Mask is mod 2 ** 16;
   Frame_Num       : constant Stack_List_Mask := 2 ** 0;
   Subprog_Name    : constant Stack_List_Mask := 2 ** 1;
   Params          : constant Stack_List_Mask := 2 ** 2;
   File_Location   : constant Stack_List_Mask := 2 ** 3;
   Program_Counter : constant Stack_List_Mask := 2 ** 4;
   All_Info        : constant Stack_List_Mask := 2 ** 5 - 1;
   --  Lists the information to be displayed in the stack list window.

   ---------------------------
   -- Debugger_Process_Tab --
   ---------------------------
   --  This type represents one of the tabs in the process notebook, and
   --  its associated debugger session.
   --  This is the graphical part of the Debugger.* packages, and all graphic
   --  subprogram calls should be done on that type instead of on a
   --  Debugger'Class.
   --  Note also that the real contents of the notebook page is not the
   --  Debugger_Process_Tab_Record itself, but rather its Process_Paned
   --  field.
   --
   --  Signals defined:
   --
   --  - "process_stopped"
   --    procedure Handler (Widget : access Debugger_Process_Tab_Record'Class);
   --
   --    Generated each time the process debugged ran and then stopped (e.g
   --    on a breakpoint, after a next command, ...).
   --
   --  - "context_changed"
   --    procedure Handler (Widget : access Debugger_Process_Tab_Record'Class);
   --
   --    Generated each time the context of the debuggee is changed (this
   --    includes thread switching, frame selection, ...).
   --
   --  - "executable_changed"
   --    procedure Handler (Widget : access Debugger_Process_Tab_Record'Class);
   --
   --    Emitted every time the executable associated with the debugger is
   --    changed (via for instance the menu Files->Open Program).
   --    This is also called initially when the executable is given on the
   --    command line.

   type Regexp_Filter_List is private;

   type Debugger_Process_Tab_Record is new
     Process_Tab_Pkg.Process_Tab_Record with
   record
      Debugger_Num            : Natural;
      --  The number identifying the debugger.

      Debugger                : Debugger_Access;
      --  The underlying debugger process.

      Window                  : Main_Debug_Window_Pkg.Main_Debug_Window_Access;
      --  The associated main window.

      Edit_Pos                : Glib.Guint;
      --  The last position in the text window of the debugger where text
      --  was inserted. This is used to find what was typed by the user.

      Debugger_Text_Highlight_Color : Gdk.Color.Gdk_Color;
      --  Color used for highlighting in the debugger window.

      Debugger_Text_Font      : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
      --  Font used in the debugger window.

      Selected_Item           : Gtkada.Canvas.Canvas_Item := null;
      Selected_Component      : Items.Generic_Type_Access := null;
      --  The currently selected item, and its specific component.

      Registered_Dialog       : Gtk.Dialog.Gtk_Dialog := null;
      --  Currently displayed dialog that should be deleted on next user input.
      --  This is mostly used for question dialogs, since the user can also
      --  type its input directly in the command window.

      Breakpoints             : GVD.Types.Breakpoint_Array_Ptr;
      --  The list of breakpoints and watchpoints currently defined.

      Has_Temporary_Breakpoint : Boolean := True;
      --  Whether there exists a temporary breakpoint in Breakpoints.

      Descriptor              : GVD.Types.Program_Descriptor;
      --  This is used to store the launching method.
      --  (Added to handle sessions)

      Input_Id                : Glib.Gint := 0;

      Backtrace_Mask : Stack_List_Mask :=
        Subprog_Name or Params;
      --  What columns to be displayed in the stack list window

      Current_Command         : String_Access;
      --  Async command currently running in the underlying debugger, if any.

      Current_Output          : String_Access;
      --  Complete output received in the underlying debugger for the current
      --  command. This is needed to buffer the output before calling the
      --  various filters.

      Post_Processing         : Boolean := False;
      --  True if the debugger is handling post processing of a command.

      Exiting                 : Boolean := False;
      --  True if the debugger is exiting.

      Filters                 : Regexp_Filter_List;
      --  List of regexp filters registered to this process.

      Last_Match              : Natural := 0;
      --  Last match in Current_Output.
      --  This is needed to avoid matching twice the same string and to
      --  optimize the handling of regexp filters.
   end record;
   type Debugger_Process_Tab is access all Debugger_Process_Tab_Record'Class;

   package Process_User_Data is new User_Data (Debugger_Process_Tab);
   --  This is used to convert from the notebook page associated with the
   --  debugger and the Debugger_Process_Tab structure.
   --  ??? This would not be required if Process_Tab_Record was directly
   --  a Gtk_VPaned, instead of a toplevel window.

   function Create_Debugger
     (Window          : access
        Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class;
      Kind            : GVD.Types.Debugger_Type;
      Executable      : String;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") return Debugger_Process_Tab;
   --  Create a debugger with a given list of arguments.
   --  A new page is added to the window's main notebook.
   --
   --  Kind specifies which debugger should be launched.
   --  Currently, only gdb and jdb are supported.
   --
   --  Executable is the name of the executable module to debug.
   --  This function returns a Process_Tab_Access.
   --
   --  Debugger_Args are the optional parameters for the underlying debugger.
   --
   --  Executable_Args are the optional parameters for the debuggee.
   --
   --  See Debugger.Spawn for a documentation on Remote_Host, Remote_Target,
   --  Remote_Protocol and Debugger_Name.

   Debugger_Not_Supported : exception;
   --  Raised by Create_Debugger when the debugger type is not supported.

   Debugger_Not_Found : exception;
   --  ??? This needs documentation.

   type Regexp_Filter_Function is access procedure
     (Process : access Debugger_Process_Tab_Record'Class;
      Str     : String;
      Match   : Match_Array);
   --  To be used with Add_Regexp_Filter below.

   procedure Add_Regexp_Filter
     (Process   : access Debugger_Process_Tab_Record'Class;
      Filter    : Regexp_Filter_Function;
      Regexp    : Pattern_Matcher);
   --  Add a new regexp filter.
   --  This filter will be run when output from a debugger is received
   --  that matches regexp.

   function Get_Num (Tab : Debugger_Process_Tab) return Glib.Gint;
   --  Return the number identifying the debugger associated with a process tab

   function Convert
     (Main_Debug_Window : access
        Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor'Class)
      return Debugger_Process_Tab;
   --  Return the debugger_descriptor associated with a Process_Descriptor.
   --  If no such page is found, an exception Debugger_Not_Found is raised.

   function Convert
     (Text : access GVD.Code_Editors.Code_Editor_Record'Class)
      return Debugger_Process_Tab;
   --  Conversion function, from the code editor to the process tab.
   --  Note that there is a single such editor per process, even if there are
   --  multiple threads/tasks.

   function Convert
     (Main_Debug_Window : access Gtk.Window.Gtk_Window_Record'Class;
      Debugger          : access Debugger_Root'Class)
      return Debugger_Process_Tab;
   --  Conversion function.
   --  Main_Debug_Window should be the window in which the debugger is
   --  displayed.

   procedure Send_Init (Process : access Debugger_Process_Tab_Record'Class);
   --  Call this procedure before each command sent to the debugger.
   --  This will take care of setting up appropriate data to handle filters
   --  properly.

   procedure Set_Busy_Cursor
     (Debugger : access Debugger_Process_Tab_Record'Class;
      Busy     : Boolean := True);
   --  Enable or disable the "busy" cursor.

   function Get_Current_Process
     (Main_Window : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Debugger_Process_Tab;
   --  Return the current process tab in Main_Window.
   --  Main_Window should be a pointer to the top-level window in gvd.
   --  This returns null if there is no current debugger.

   procedure Process_Stopped
     (Debugger : access Debugger_Process_Tab_Record'Class);
   --  Emit the "process_stopped" signal.

   procedure Context_Changed
     (Debugger : access Debugger_Process_Tab_Record'Class);
   --  Emit the "context_changed" signal.

   procedure Executable_Changed
     (Debugger : access Debugger_Process_Tab_Record'Class;
      Executable_Name : String);
   --  Emit the "executable_changed" signal.
   --  This basically warns all listeners that the associated debugger is now
   --  editing a file called Executable_Name

   procedure Close_Debugger
     (Debugger : Debugger_Process_Tab);
   --  Close the debugger, remove the notebook page and modify the commmands
   --  history accordingly.

   procedure Process_User_Command
     (Debugger       : Debugger_Process_Tab;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Visible_Command := GVD.Types.Visible);
   --  Process a command entered by the user.
   --  In most cases, the command is simply transfered asynchronously to the
   --  debugger process. However, commands internal to GVD are filtered and
   --  are not transmitted to the debugger.
   --  If Output_Command is True, then the command is first output to the
   --  command window. An ASCII.LF is appended at the end before printing.
   --  Mode is the type of the command which will be transmitted to the
   --  debugger.

   procedure Output_Text
     (Process      : Debugger_Process_Tab;
      Str          : String;
      Is_Command   : Boolean := False;
      Set_Position : Boolean := False);
   --  Insert Str in the debugger window.
   --  Note that this function does not change the Edit_Pos for the record,
   --  so this should be used only for temporary output.
   --  This also does some highlighting if the debugger supports highlighting.
   --  If Is_Command is True, then the string is displayed in the highlighting
   --  color, used for user commands.
   --  If Set_Position is True, then the cursor position after the text has
   --  been inserted is considered as the beginning of a new command. The user
   --  will be able to delete text back to that position, but not before, and
   --  when <enter> is pressed, the text from the position onwards is sent to
   --  the debugger.

   procedure Register_Dialog
     (Process : access Debugger_Process_Tab_Record;
      Dialog  : access Gtk.Dialog.Gtk_Dialog_Record'Class);
   --  Register a dialog, that will be deleted next time a user command is
   --  processed. Only one such dialog can be registered at any given time.
   --  Program_Error is raised if there is already such a dialog.

   procedure Unregister_Dialog (Process : access Debugger_Process_Tab_Record);
   --  Destroy any registered dialog.
   --  Nothing happens if there is no such dialog.

   --------------------------
   -- Breakpoints handling --
   --------------------------

   procedure Update_Breakpoints
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force  : Boolean);
   --  Update the list of breakpoints every time the process is stopped.
   --  This also updates all the visual windows where the breakpoints are
   --  displayed.
   --  Not that the list of breakpoints is not reparsed if Force is False and
   --  there is no temporary breakpoint in the current list.

   function Toggle_Breakpoint_State
     (Process        : access Debugger_Process_Tab_Record;
      Breakpoint_Num : Integer) return Boolean;
   --  Toggle the enabled/disabled state of a specific breakpoint in the
   --  current process, and return the new state.
   --  The internal list of breakpoints is also updated, but no command is
   --  emitted to the debugger.
   --  False is returned when there is no such breakpoint in the list (or the
   --  list of breakpoints has never been parsed before).

private

   type Pattern_Matcher_Access is access Pattern_Matcher;

   type Regexp_Filter_List_Elem;
   type Regexp_Filter_List is access Regexp_Filter_List_Elem;
   type Regexp_Filter_List_Elem is record
      Filter : Regexp_Filter_Function;
      Regexp : Pattern_Matcher_Access;
      Next   : Regexp_Filter_List;
   end record;

end GVD.Process;
