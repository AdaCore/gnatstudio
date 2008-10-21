-----------------------------------------------------------------------
--                                GPS                                --
--                                                                   --
--               Copyright (C) 2000-2008, AdaCore                    --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Glib.Object;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Expect; use GNAT.Expect;

with Gtk.Dialog;
with Gtk.Main;
with Gtk.Widget;

with Debugger;            use Debugger;
with GPS.Kernel;
with GPS.Main_Window;
with GVD.Code_Editors;
with GVD.Types;
with GVD.Histories;
with Projects;
pragma Elaborate_All (GVD.Histories);
with GNATCOLL.VFS;

with Interactive_Consoles; use Interactive_Consoles;

package GVD.Process is

   type History_Data is record
      Mode    : GVD.Types.Command_Type;
      Command : String_Access;
   end record;

   package String_History is new GVD.Histories (History_Data);
   use String_History;

   ---------------------
   -- Visual_Debugger --
   ---------------------
   --  This type represents a graphical debugger, and its associated debugger
   --  session.
   --  This is the graphical part of the Debugger.* packages, and all graphic
   --  subprogram calls should be done on that type instead of on a
   --  Debugger'Class.

   type Regexp_Filter_List is private;

   type Visual_Debugger_Record is
     new Glib.Object.GObject_Record with
   record
      Editor_Text             : GVD.Code_Editors.Code_Editor;
      Debugger_Num            : Natural;
      --  The number identifying the debugger.

      Debugger                : Debugger_Access;
      --  The underlying debugger process.

      Window                  : GPS.Main_Window.GPS_Window;
      --  The associated main window.

      Command_History         : String_History.History_List;
      --  The history of commands for the current session.

      Debugger_Text           : Interactive_Console;
      Debuggee_Console        : Interactive_Console;
      --  Separate console for debugged programs, if debugger supports ttys.
      --  See gvd-consoles.adb for these two fields

      Stack                   : Gtk.Widget.Gtk_Widget;
      Threads                 : Gtk.Widget.Gtk_Widget;
      Tasks                   : Gtk.Widget.Gtk_Widget;
      PDs                     : Gtk.Widget.Gtk_Widget;
      Data                    : Gtk.Widget.Gtk_Widget;
      Assembly                : Gtk.Widget.Gtk_Widget;
      --  The call stack, threads, task, protection domains, data and assembly
      --  views.

      Breakpoints             : GVD.Types.Breakpoint_Array_Ptr;
      --  The list of breakpoints and watchpoints currently defined.

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
      --  various filters. It is only initialized in the range
      --  Current_Output'First .. Current_Output_Pos - 1

      Exiting                 : Boolean := False;
      --  True if the debugger is exiting.

      Interactive_Command    : Boolean := False;
      --  True if the current command was typed manually by the user in the
      --  console.

      ------------
      --  private fields
      ------------
      --  The following fields should only be used in gvd-process.adb

      Registered_Dialog       : Gtk.Dialog.Gtk_Dialog := null;
      --  Currently displayed dialog that should be deleted on next user input.
      --  This is mostly used for question dialogs, since the user can also
      --  type its input directly in the command window.

      Has_Temporary_Breakpoint : Boolean := True;
      --  Whether there exists a temporary breakpoint in Breakpoints.

      Current_Output_Pos      : Natural := 1;
      --  Position in Current_Output to insert new text.

      Post_Processing         : Boolean := False;
      --  True if the debugger is handling post processing of a command.

      Filters                 : Regexp_Filter_List;
      --  List of regexp filters registered to this process.

      Last_Match              : Natural := 0;
      --  Last match in Current_Output.
      --  This is needed to avoid matching twice the same string and to
      --  optimize the handling of regexp filters.

      Current_File            : GNATCOLL.VFS.Virtual_File;
      --  The file containing the current location.

      Current_Line            : Integer := 0;
      --  The current line in Current_File.

      Pc                      : GVD.Types.Address_Type :=
                                  GVD.Types.Invalid_Address;
   end record;
   type Visual_Debugger is access all Visual_Debugger_Record'Class;

   function Spawn
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Projects.Project_Type;
      Args    : String) return Visual_Debugger;
   --  Spawn a new debugger on File (taking into account the settings from
   --  Project). Args are passed to the executable File.

   procedure Close_Debugger (Process : access Visual_Debugger_Record);
   --  Close the given debugger and terminate the debugging session if this
   --  is the last one.

   function Get_Kernel
     (Process : access Visual_Debugger_Record'Class)
      return GPS.Kernel.Kernel_Handle;
   --  Return the GPS kernel

   procedure Load_Project_From_Executable
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Visual_Debugger_Record'Class);
   --  Create and load a default project from the executable loaded in
   --  Debugger.

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

   function Get_Console
     (Process : access Visual_Debugger_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Return the debugger console associated with Process

   function Convert
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor'Class)
      return Visual_Debugger;
   --  Return the debugger_descriptor associated with a Process_Descriptor.
   --  If no such page is found, an exception Debugger_Not_Found is raised.

   function Convert
     (Debugger : access Debugger_Root'Class)
      return Visual_Debugger;
   --  Conversion function.
   --  Main_Debug_Window should be the window in which the debugger is
   --  displayed.

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

   function Command_In_Process
     (Debugger : access Visual_Debugger_Record'Class) return Boolean;
   --  Return True if a command is currently being processed, and thus no
   --  other command can be sent to the debugger

   procedure Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Command_Type := GVD.Types.Visible);
   --  Process a command entered by the user.
   --  In most cases, the command is simply transfered asynchronously to the
   --  debugger process. However, commands internal to GVD are filtered and
   --  are not transmitted to the debugger.
   --  If Output_Command is True, then the command is first output to the
   --  command window. An ASCII.LF is appended at the end before printing.
   --  Mode is the type of the command which will be transmitted to the
   --  debugger.

   function Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String;
   --  Same as above, but returns the debugger output

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

   --------------
   -- Commands --
   --------------

   function Get_Command
     (Process : access Visual_Debugger_Record'Class) return String;
   function Is_Execution_Command
     (Process : access Visual_Debugger_Record'Class) return Boolean;
   --  Return the command currently executed in the debugger, and whether it is
   --  likely to change the callstack when it finishes its execution

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

   ---------------------
   -- Source location --
   ---------------------

   procedure Set_Current_Source_Location
     (Process : access Visual_Debugger_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Integer);
   --  Set the source location.

   function Get_Current_Source_File
     (Process : access Visual_Debugger_Record)
      return GNATCOLL.VFS.Virtual_File;
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
