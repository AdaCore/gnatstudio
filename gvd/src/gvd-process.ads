------------------------------------------------------------------------------
--                                   GPS                                    --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Glib;

with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GNAT.Regpat;          use GNAT.Regpat;
with GNAT.Expect;          use GNAT.Expect;

with Gtk.Widget;

with Commands.Interactive; use Commands.Interactive;
with Debugger;             use Debugger;
with Generic_Views;        use Generic_Views;
with GPS.Debuggers;        use GPS.Debuggers;
with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with GVD.Breakpoints_List; use GVD.Breakpoints_List;
with GVD.Types;
with GVD.Histories;
pragma Elaborate_All (GVD.Histories);
with GNATCOLL.Projects;    use GNATCOLL.Projects;
with GNATCOLL.VFS;
with GPS.Dialogs;          use GPS.Dialogs;
with GNAT.TTY;

package GVD.Process is

   type History_Data is record
      Mode    : GVD.Types.Command_Type;
      Command : String_Access;
   end record;

   package String_History is new GVD.Histories (History_Data);
   use String_History;

   Debug_Queue_Name : constant String := "debugger output monitor";
   --  Name to use for the command queue that contains the command which
   --  monitors the debug output.

   ---------------------
   -- Visual_Debugger --
   ---------------------
   --  This type represents a graphical debugger, and its associated debugger
   --  session.
   --  This is the graphical part of the Debugger.* packages, and all graphic
   --  subprogram calls should be done on that type instead of on a
   --  Debugger'Class.

   type Regexp_Filter_List is private;

   type Visual_Debugger_Record is new Base_Visual_Debugger with record
      Debugger_Num            : Natural;
      --  The number identifying the debugger.

      Debugger                : Debugger_Access;
      --  The underlying debugger process.

      Kernel                  : GPS.Kernel.Kernel_Handle;
      --  The associated kernel.

      Command_History         : String_History.History_List;
      --  The history of commands for the current session.

      Debugger_Text           : Generic_Views.Abstract_View_Access;
      Debuggee_Console        : Generic_Views.Abstract_View_Access;
      Debuggee_TTY            : GNAT.TTY.TTY_Handle;
      --  tty for Debugger Execution console
      Stack                   : Generic_Views.Abstract_View_Access;
      Threads                 : Generic_Views.Abstract_View_Access;
      Tasks                   : Generic_Views.Abstract_View_Access;
      PDs                     : Generic_Views.Abstract_View_Access;
      Data                    : Generic_Views.Abstract_View_Access;
      Assembly                : Generic_Views.Abstract_View_Access;
      Breakpoints_Editor      : Generic_Views.Abstract_View_Access;
      Memory_View             : Generic_Views.Abstract_View_Access;
      Variables_View          : Generic_Views.Abstract_View_Access;
      Registers_View          : Generic_Views.Abstract_View_Access;
      --  All views potentially associated with a debugger

      Breakpoints             : aliased GVD.Breakpoints_List.Breakpoint_List;
      --  The list of breakpoints and watchpoints specific to this debugger.

      Avoid_Breakpoints_Copy  : Boolean := False;
      --  True if the copy of the debugger's internal breakpoint's list to
      --  the persistent's one should be avoided. This can be the case when
      --  some persistent breakpoints are not recognized by the debugger.

      Descriptor              : GVD.Types.Program_Descriptor;
      --  This is used to store the launching method.
      --  (Added to handle sessions)

      Current_Command         : String_Access;
      --  Async command currently running in the underlying debugger, if any.

      Current_Output          : String_Access;
      --  Complete output received in the underlying debugger for the current
      --  command. This is needed to buffer the output before calling the
      --  various filters. It is only initialized in the range
      --  Current_Output'First .. Current_Output_Pos - 1

      Current_Output_Pos      : Natural := 1;
      --  Position in Current_Output to insert new text.

      Exiting                 : Boolean := False;
      --  True if the debugger is exiting.

      Is_From_Dbg_Console     : Boolean := False;
      --  True if the current command was issued by the user from the console.
      --  Valid both for command typed manually, and for commands issued by
      --  shortcuts while the focus is in the console

      Current_File            : GNATCOLL.VFS.Virtual_File;
      Current_Line            : Natural := 0;
      --  The file/line on which the debugger is stopped (ie these were set
      --  when the Set_Current parameter is True for Set_line and Load_File)

      ------------
      --  private fields
      ------------
      --  The following fields should only be used in gvd-process.adb

      Registered_Dialog       : access GPS_Dialog_Record'Class := null;
      --  Currently displayed dialog that should be deleted on next user input.
      --  This is mostly used for question dialogs, since the user can also
      --  type its input directly in the command window.

      Post_Processing         : Boolean := False;
      --  True if the debugger is handling post processing of a command.

      Filters                 : Regexp_Filter_List;
      --  List of regexp filters registered to this process.

      Log_Lines : Natural := 0;
      --  Number of lines output in the log file, while executing commands
      --  with gdb (see gvd-trace.ads). We only log some of the output.

      Last_Match              : Natural := 0;
      --  Last match in Current_Output.
      --  This is needed to avoid matching twice the same string and to
      --  optimize the handling of regexp filters.

      Pc : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
   end record;
   type Visual_Debugger is access all Visual_Debugger_Record'Class;

   function Spawn
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Kind            : GVD.Types.Debugger_Type;
      File            : GNATCOLL.VFS.Virtual_File;
      Project         : Project_Type;
      Args            : String;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Load_Executable : Boolean := False) return Visual_Debugger;
   --  Spawn a new debugger on File (taking into account the settings from
   --  Project). Args are passed to the executable File.
   --
   --  If non-empty, Remote_Target and Remote_Protocol are used to initialize
   --  a remote connection instead of using the attributes defined in the IDE
   --  package for this purpose (i.e: respectively IDE'Program_Host and
   --  IDE'Communication_Protocol). Load_Executable is then used to know
   --  whether GPS should automatically load the debugged executable on the
   --  remote target.

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

   overriding function Get_Num
     (Self : not null access Visual_Debugger_Record) return Glib.Gint;
   overriding function Command_In_Process
     (Self : not null access Visual_Debugger_Record) return Boolean;

   function Get_Console
     (Process : access Visual_Debugger_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Return the debugger console associated with Process

   function Convert
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor'Class)
      return Visual_Debugger;
   --  Return the debugger_descriptor associated with a Process_Descriptor.
   --  If no such page is found, returns null

   function Convert
     (Debugger : access Debugger_Root'Class)
      return Visual_Debugger;
   --  Conversion function.

   procedure Final_Post_Process
     (Process           : not null access Visual_Debugger_Record'Class;
      Mode              : GVD.Types.Command_Type;
      Always_Emit_Hooks : Boolean;
      Category          : Command_Category;
      Breakpoints_Might_Have_Changed : Boolean);
   --  Final post processing.
   --  Call the appropriate filters and reset Current_Output.
   --  The hooks reporting the change of state of the debugger are only emited
   --  when the mode is not Internal. But if Always_Emit_Hooks is true, they
   --  are always emitted.
   --  Breakpoints_Might_Have_Changed should be set to True if the previous
   --  command might have changed any of the breakpoints. This will force GPS
   --  to query the list again, and send appropriate signals.

   procedure Process_User_Command
     (Debugger       : not null access Visual_Debugger_Record'Class;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Command_Type := GVD.Types.Visible);
   --  Process a command entered by the user.
   --  The debugger must not be busy processing another command (in general,
   --  it is better to use Send, which will enqueue as needed).
   --  In most cases, the command is simply transfered asynchronously to the
   --  debugger process. However, commands internal to GVD are filtered and
   --  are not transmitted to the debugger.
   --  If Output_Command is True, then the command is first output to the
   --  command window. An ASCII.LF is appended at the end before printing.
   --  Mode is the type of the command which will be transmitted to the
   --  debugger.

   function Process_User_Command
     (Debugger       : not null access Visual_Debugger_Record'Class;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String;
   --  Same as above, but returns the debugger output

   procedure Output_Text
     (Process      : not null access Visual_Debugger_Record'Class;
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
      Dialog  : access GPS_Dialog_Record'Class);
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
   --  Return the command currently executed in the debugger

   function Is_Execution_Command
     (Process : access Visual_Debugger_Record'Class) return Boolean;
   --  Return whether current command is likely to change the callstack when it
   --  finishes its execution

   procedure Create_Execution_Console
     (Process : access Visual_Debugger_Record'Class);
   --  Creates Debugger Execution console

   --------------
   -- Commands --
   --------------
   --  This subpackage defines a new Debugger_Command type, that is used as a
   --  basis for every debugger command type

   package Dbg_Command is
      type Debugger_Command is abstract new Interactive_Command
      with null record;
      type Debugger_Command_Access is access all Debugger_Command'Class;
      --  Abstract type that is the basis for debugger commands. Will take care
      --  of some boilerplate code, like checking that the debugger is active,
      --  and checking wether the command has been issued from the debugger
      --  console Debugger commands don't need to override Execute, as is
      --  usual with Interactive_Commands, but Execute_Dbg

      overriding function Execute
        (Command : access Debugger_Command;
         Context : Interactive_Command_Context)
        return Commands.Command_Return_Type;
      --  Overridden Execute primitive to take care of Debugger_Command
      --  boilerplate, do not override

      function Execute_Dbg
        (Command : access Debugger_Command;
         Process : Visual_Debugger)
        return Commands.Command_Return_Type is abstract;
      --  Types derived from Debugger_Command need to override this primitive
      --  Process is the process of the active debugger for the command.

   end Dbg_Command;

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
