-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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
with Glib.Object;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Expect; use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;
with GNAT.TTY;
pragma Warnings (On);

with Gdk.Color;
with Gtk.Menu;
with Gtk.Dialog;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Main;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Window;
with Gtk.Widget;
with Gtkada.Canvas;       use Gtkada.Canvas;

with Pango.Font;

with Process_Proxies;     use Process_Proxies;
with Debugger;            use Debugger;
with GVD.Main_Window;
with Items;
with GVD.Code_Editors;
with GVD.Text_Box.Source_Editor;
with GVD.Types;
with Histories;

with Interactive_Consoles; use Interactive_Consoles;

with GVD.Call_Stack;       use GVD.Call_Stack;

package GVD.Process is

   Command_History_Collapse_Entries : constant Boolean := True;
   --  Whether we should collapse entries in the history list.

   ---------------------
   -- Visual_Debugger --
   ---------------------
   --  This type represents a graphical debugger, and its associated debugger
   --  session.
   --  This is the graphical part of the Debugger.* packages, and all graphic
   --  subprogram calls should be done on that type instead of on a
   --  Debugger'Class.
   --
   --  Signals defined:
   --
   --  - "process_stopped"
   --    procedure Handler (Widget : access Visual_Debugger_Record'Class);
   --
   --    Generated each time the process debugged ran and then stopped (e.g
   --    on a breakpoint, after a next command, ...).
   --
   --  - "context_changed"
   --    procedure Handler (Widget : access Visual_Debugger_Record'Class);
   --
   --    Generated each time the context of the debuggee is changed (this
   --    includes thread switching, frame selection, ...).
   --
   --  - "executable_changed"
   --    procedure Handler (Widget : access Visual_Debugger_Record'Class);
   --
   --    Emitted every time the executable associated with the debugger is
   --    changed (via for instance the menu Files->Open Program).
   --    This is also called initially when the executable is given on the
   --    command line.
   --
   --  - "debugger_closed"
   --    procedure Handler (Widget : access Visual_Debugger_Record'Class);
   --
   --    Emitted whenever the underlying debugger is closed.


   type Regexp_Filter_List is private;

   type Visual_Debugger_Record is
     new Glib.Object.GObject_Record with
   record
      Editor_Text             : GVD.Code_Editors.Code_Editor;
      Debugger_Num            : Natural;
      --  The number identifying the debugger.

      Debugger                : Debugger_Access;
      --  The underlying debugger process.

      Window                  : GVD.Main_Window.GVD_Main_Window;
      --  The associated main window.

      Delete_Text_Handler_Id  : Gtk.Handlers.Handler_Id;
      Stack_List_Select_Id    : Gtk.Handlers.Handler_Id;
      Destroy_Id              : Gtk.Handlers.Handler_Id;

      Stack                   : GVD.Call_Stack.Call_Stack;

      Data_Scrolledwindow     : Gtk_Scrolled_Window;
      Data_Canvas             : Interactive_Canvas;

      Debugger_Text           : Interactive_Console;
      Debugger_Text_Font      : Pango.Font.Pango_Font_Description;
      Debugger_Text_Highlight_Color : Gdk.Color.Gdk_Color;

      Debuggee_Console        : Interactive_Console;
      --  Separate console for debugged programs, if debugger supports ttys

      Debuggee_TTY            : GNAT.TTY.TTY_Handle;
      Debuggee_Descriptor     : GNAT.Expect.TTY.TTY_Process_Descriptor;
      Debuggee_Id             : Gtk.Main.Timeout_Handler_Id := 0;
      Cleanup_TTY             : Boolean := False;

      History                 : Histories.History;
      --  See Glide_Kernel.Get_History. This points to the same one as the GPS
      --  kernel if we are not in standalone mode

      Edit_Pos                : Glib.Guint;
      --  The last position in the text window of the debugger where text
      --  was inserted. This is used to find what was typed by the user.

      Selected_Item           : Gtkada.Canvas.Canvas_Item := null;
      Selected_Component      : Items.Generic_Type_Access := null;
      --  The currently selected item, and its specific component.

      Registered_Dialog       : Gtk.Dialog.Gtk_Dialog := null;
      --  Currently displayed dialog that should be deleted on next user input.
      --  This is mostly used for question dialogs, since the user can also
      --  type its input directly in the command window.

      Continuation_Line       : Boolean := False;
      --  Whether the debugger is currently handling a multiple line command.

      Breakpoints             : GVD.Types.Breakpoint_Array_Ptr;
      --  The list of breakpoints and watchpoints currently defined.

      Has_Temporary_Breakpoint : Boolean := True;
      --  Whether there exists a temporary breakpoint in Breakpoints.

      Descriptor              : GVD.Types.Program_Descriptor;
      --  This is used to store the launching method.
      --  (Added to handle sessions)

      Timeout_Id              : Gtk.Main.Timeout_Handler_Id := 0;
      --  Timeout Id used to handle async. commands.

      Current_Command         : String_Access;
      --  Async command currently running in the underlying debugger, if any.

      Current_Output          : String_Access;
      --  Complete output received in the underlying debugger for the current
      --  command. This is needed to buffer the output before calling the
      --  various filters.

      Current_Output_Pos      : Natural := 1;
      --  Position in Current_Output to insert new text.

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

      Contextual_Menu         : Gtk.Menu.Gtk_Menu;

      Separate_Data           : Boolean;
      --  Store current value of the Separate_Data preference.

      Current_File            : String_Access;
      --  The file containing the current location.

      Current_Line            : Integer := 0;
      --  The current line in Current_File.
   end record;
   type Visual_Debugger is access all Visual_Debugger_Record'Class;

   procedure Gtk_New
     (Process : out Visual_Debugger;
      Window  : access GVD.Main_Window.GVD_Main_Window_Record'Class;
      Source  : GVD.Text_Box.Source_Editor.Source_Editor);
   --  Create a new debugger page and add it to Window.

   procedure Initialize
     (Process : access Visual_Debugger_Record'Class;
      Window  : access GVD.Main_Window.GVD_Main_Window_Record'Class;
      Source  : GVD.Text_Box.Source_Editor.Source_Editor);
   --  Internal initialize procedure.

   procedure Configure
     (Process         : access Visual_Debugger_Record'Class;
      Kind            : GVD.Types.Debugger_Type;
      Proxy           : Process_Proxy_Access;
      Executable      : String;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "";
      History         : Histories.History := null;
      Success         : out Boolean);
   --  Configure a process tab.
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
   --
   --  Success is set to true is the debugger could be successfully started.

   Debugger_Not_Supported : exception;
   --  Raised by Create_Debugger when the debugger type is not supported.

   Debugger_Not_Found : exception;
   --  Raised by Convert when no debugger is found.

   type Regexp_Filter_Function is access procedure
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Match   : Match_Array);
   --  To be used with Add_Regexp_Filter below.

   procedure Add_Regexp_Filter
     (Process : access Visual_Debugger_Record'Class;
      Filter  : Regexp_Filter_Function;
      Regexp  : Pattern_Matcher);
   --  Add a new regexp filter.
   --  This filter will be run when output from a debugger is received
   --  that matches regexp.

   function Get_Num (Tab : Visual_Debugger) return Glib.Gint;
   --  Return the number identifying the debugger associated with a process tab

   function Convert
     (Main_Debug_Window : access GVD.Main_Window.GVD_Main_Window_Record'Class;
      Descriptor        : GNAT.Expect.Process_Descriptor'Class)
      return Visual_Debugger;
   --  Return the debugger_descriptor associated with a Process_Descriptor.
   --  If no such page is found, an exception Debugger_Not_Found is raised.

   function Convert
     (Main_Debug_Window : access Gtk.Window.Gtk_Window_Record'Class;
      Debugger          : access Debugger_Root'Class)
      return Visual_Debugger;
   --  Conversion function.
   --  Main_Debug_Window should be the window in which the debugger is
   --  displayed.

   procedure Create_Call_Stack (Process : access Visual_Debugger_Record'Class);
   --  Create the call stack widget associated with Process.

   procedure Final_Post_Process
     (Process : access Visual_Debugger_Record'Class;
      Mode    : GVD.Types.Command_Type);
   --  Final post processing.
   --  Call the appropriate filters and reset Current_Output.

   procedure Set_Busy
     (Debugger      : access Visual_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False);
   --  Enable or disable the "busy" cursor.
   --  If Force_Refresh is True, then the pending X11 events are immediately
   --  processed so that the pointer becomes visible right away

   function Get_Current_Process
     (Main_Window : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Visual_Debugger;
   --  Return the current process tab in Main_Window.
   --  Main_Window should be a pointer to the top-level window in gvd.
   --  This returns null if there is no current debugger.

   procedure Process_Stopped
     (Debugger : access Visual_Debugger_Record'Class);
   --  Emit the "process_stopped" signal.

   procedure Context_Changed
     (Debugger : access Visual_Debugger_Record'Class);
   --  Emit the "context_changed" signal.

   procedure Executable_Changed
     (Debugger : access Visual_Debugger_Record'Class;
      Executable_Name : String);
   --  Emit the "executable_changed" signal.
   --  This basically warns all listeners that the associated debugger is now
   --  editing a file called Executable_Name

   procedure Close_Debugger (Debugger : Visual_Debugger);
   --  Close the debugger, remove the notebook page and modify the commmands
   --  history accordingly.
   --  Emit the "debugger_closed" signal.

   procedure Process_User_Command
     (Debugger       : Visual_Debugger;
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
     (Process      : Visual_Debugger;
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
     (Process : access Visual_Debugger_Record;
      Dialog  : access Gtk.Dialog.Gtk_Dialog_Record'Class);
   --  Register a dialog, that will be deleted next time a user command is
   --  processed. Only one such dialog can be registered at any given time.
   --  Program_Error is raised if there is already such a dialog.

   procedure Unregister_Dialog (Process : access Visual_Debugger_Record);
   --  Destroy any registered dialog.
   --  Nothing happens if there is no such dialog.

   --------------------------
   -- Breakpoints handling --
   --------------------------

   procedure Update_Breakpoints
     (Process : access Glib.Object.GObject_Record'Class;
      Force   : Boolean);
   --  Update the list of breakpoints every time the process is stopped.
   --  This also updates all the visual windows where the breakpoints are
   --  displayed.
   --  Not that the list of breakpoints is not reparsed if Force is False and
   --  there is no temporary breakpoint in the current list.

   function Toggle_Breakpoint_State
     (Process        : access Visual_Debugger_Record;
      Breakpoint_Num : GVD.Types.Breakpoint_Identifier) return Boolean;
   --  Toggle the enabled/disabled state of a specific breakpoint in the
   --  current process, and return the new state.
   --  The internal list of breakpoints is also updated, but no command is
   --  emitted to the debugger.
   --  False is returned when there is no such breakpoint in the list (or the
   --  list of breakpoints has never been parsed before).

   procedure Update_Editor_Frame
     (Process : access Visual_Debugger_Record);
   --  Update the editor frame with the name of the currently edited file.

   ---------------------
   -- Source location --
   ---------------------

   procedure Set_Current_Source_Location
     (Process : access Visual_Debugger_Record;
      File    : String;
      Line    : Integer);
   --  Set the source location.

   function Get_Current_Source_File
     (Process : access Visual_Debugger_Record) return String;
   --  Get the file containing the current location, or "" if there is none.

   function Get_Current_Source_Line
     (Process : access Visual_Debugger_Record) return Integer;
   --  Get the current line, or 0 if there is none.

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
