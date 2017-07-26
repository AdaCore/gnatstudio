------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.VFS;

with Language;
with Items;
with Process_Proxies;
with Basic_Types;              use Basic_Types;
with GVD.Breakpoints_List;     use GVD.Breakpoints_List;
with GVD.Types;
with GVD.Proc_Utils;
with GPS.Kernel;               use GPS.Kernel;

package Debugger is

   type Debugger_Root is abstract tagged private;
   --  The general base class for all debuggers.
   --  Each debugger should extend this base class.

   type Debugger_Access is access all Debugger_Root'Class;

   procedure Spawn
     (Debugger        : access Debugger_Root;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") is abstract;
   --  Spawn the external process.
   --  Initialize should be called afterwards, but this is done in two
   --  separate steps so that it is possible to set filters.
   --
   --  Executable is the name of the module to debug.
   --
   --  Debugger_Args are additional arguments to pass to the debugger.
   --
   --  Executable_Args are arguments to pass to the debuggee.
   --
   --  Proxy is assigned to the debugger, after its underlying process has
   --  been created.
   --
   --  If Remote_Host is different from the empty string, the debugger
   --  is spawned on the remote host specified.
   --
   --  If Remote_Target is not empty, the debugger will, if supported, connect
   --  to the specified target using Remote_Protocol.
   --
   --  If Debugger_Name is not empty, use this name rather than a default as
   --  the executable name of the debugger.
   --
   --  The Debugger should set up filters to handle the change of current
   --  language, ...
   --
   --  Should raise Spawn_Error if the debugger could not be spawned.

   procedure General_Spawn
     (Debugger       : access Debugger_Root'Class;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Arguments      : GNAT.Strings.String_List;
      Debugger_Name  : String;
      Proxy          : Process_Proxies.Process_Proxy_Access);
   --  Convenience function to start a debugger.
   --  This command modifies the argument list so that the debugger can also
   --  be run on a remote machine.
   --  This is provided as a support for the implementation of the primitive
   --  subprogram Spawn, and should work with most debuggers.
   --
   --  Raises Spawn_Error if the debugger could not be spawned.
   --  ??? It would be nice to change the profile to use Command_Lines.CL
   --  instead of a String_List.

   procedure Initialize (Debugger : access Debugger_Root) is abstract;
   --  Initialize the debugger.
   --  Spawn must have been called first.
   --  The initial prompt of the debugger will be displayed on the debugger
   --  console, if any.

   procedure Connect_To_Target
     (Debugger : access Debugger_Root;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : GVD.Types.Invisible_Command := GVD.Types.Hidden) is null;
   --  If supported by the debugger, connect to the given target, using
   --  the given communication protocol.
   --  If Force is True, the debugger should kill any existing connection
   --  before attempting to connect to this target.

   function Is_Connected_To_Target
     (Debugger : access Debugger_Root) return Boolean is abstract;
   --  Return True if the debugger is already connected to a target, False
   --  otherwise.

   procedure Send
     (Debugger        : access Debugger_Root;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden);
   --  Send a command to the underlying process associated with Debugger.
   --  If Empty_Buffer is True, any input waiting from the process (or in the
   --  buffer) is first discarded before the command is sent.
   --  Call Wait_Prompt before exiting if Wait_For_Prompt is True.
   --  If Mode <= Hidden, then the output of the command won't be shown
   --  in the command window.
   --  If Mode indicates a visible command, it is executed asynchronously,
   --  otherwise it is executed synchronously, ie wait until we get the
   --  prompt.
   --  If a command is already executing, Cmd will be queued and executed
   --  afterward. However, in some cases it is necessary to immediately send
   --  Cmd to the debugger (for instance because the latter is waiting for
   --  input). In this case, Force_Send might be set to True
   --  Modules can define their own commands ("graph display ...") which are
   --  intercepted via the Debugger_Command_Action_Hook.

   function Send_And_Get_Output
     (Debugger        : access Debugger_Root;
      Cmd             : String;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden)
      return String;
   --  Same as above, but also return the output of the debugger.
   --  The full output is returned, ie this includes the final prompt.
   --  You should rather use the function Send below.
   --  Note that any input waiting from the process is first discarded before
   --  sending the command.
   --  You should always use this function instead of using Expect_Out
   --  yourself after calling the procedure Send, since some intermediate
   --  hidden calls to the debugger might have taken place in the meanwhile.

   function Send_And_Get_Clean_Output
     (Debugger : access Debugger_Root;
      Cmd      : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
      return String is abstract;
   --  Same Send_And_Get_Output, but return a clean version of the output, i.e.
   --  delete the final prompt if any, depending on the debugger type.

   procedure Wait_User_Command (Debugger : access Debugger_Root);
   --  Wait until the current user command ends.
   --  This is useful in particular when handling batches of command,
   --  e.g when replaying sessions or a set of user commands.

   procedure Clear_Queue (Debugger : access Debugger_Root'Class);
   --  Clear the queue of commands to execute associated with Debugger.

   function Highlighting_Pattern
     (Debugger : access Debugger_Root)
      return GNAT.Regpat.Pattern_Matcher is abstract;
   --  Return a regular expression that should match everything that should
   --  be highlighted in the debugger text window.

   procedure Close (Debugger : access Debugger_Root);
   --  Terminates the external process and clean up associated data.

   function Get_Process
     (Debugger : access Debugger_Root)
      return Process_Proxies.Process_Proxy_Access;
   --  Return the process descriptor associated with Debugger.

   procedure Set_Language
     (Debugger     : access Debugger_Root;
      The_Language : access Language.Language_Root'Class);
   --  Set the language associated with a debugger.

   function Get_Language
     (Debugger : access Debugger_Root;
      Lang     : String := "") return Language.Language_Access;
   --  Return the language_access for a specific language ("ada", "c", ...).
   --  This might return null if Set_Language was never called for that
   --  language. If Lang is the empty string, returns the current language.

   procedure Detect_Language (Debugger : access Debugger_Root) is null;
   --  Try to detect the current language associated with the debugger.

   function Parse_Type
     (Debugger : access Debugger_Root'Class;
      Entity   : String) return Items.Generic_Type_Access;
   --  Parse the type definition for Entity, and return a
   --  tree as explained in Generic_Values.

   type Value_Format is (Default_Format, Decimal, Binary, Hexadecimal, Octal);

   procedure Parse_Value
     (Debugger    : access Debugger_Root'Class;
      Entity      : String;
      Value       : in out Items.Generic_Type_Access;
      Format      : Value_Format := Default_Format;
      Value_Found : out Boolean);
   --  Parse the value of Entity.
   --  Value should contain the result of Parse_Type when this procedure is
   --  called, and it is completed to reflect the new value.
   --  Value_Found is set to True only if a valid Value could be found for the
   --  variable.

   function Get_Uniq_Id
     (Debugger : access Debugger_Root; Entity : String) return String;
   --  Return a uniq ID for Entity.
   --  In most cases, this will be the address of the variable. However, some
   --  languages do not have addresses (Java), but since they do not allow
   --  overloading, returning the name might be enough (This is the default)
   --
   --  This ID is used to detect aliases in the canvas display.
   --  The return result be the value of access types (ie types that can be
   --  dereferenced).

   procedure Wait_Prompt
     (Debugger : access Debugger_Root) is abstract;
   --  Wait for the prompt.

   function Wait_Prompt
     (Debugger : access Debugger_Root;
      Timeout  : Integer) return Boolean is abstract;
   --  Wait for the prompt.
   --  Timeout is the number of ms to wait.
   --  Return True if a prompt was found.

   procedure Display_Prompt
     (Debugger : access Debugger_Root) is abstract;
   --  Send a command to the debugger, so that the prompt is displayed
   --  again in the debugger window. This is used after internal commands like
   --  "graph print", to indicate that the command has finished executing.

   function Type_Of
     (Debugger : access Debugger_Root;
      Entity   : String) return String is abstract;
   --  Return the type of the entity.
   --  An empty string is returned if the entity is not defined in the
   --  current context.
   --  GDB_COMMAND: "ptype"

   function Get_Type_Info
     (Debugger  : access Debugger_Root;
      Entity    : String;
      Default   : String) return String;
   --  Return a string suitable for printing in canvas items for the type.
   --  Default is the result of Type_Of for Entity, and should be returned
   --  if the debugger does not have any special support for this command.
   --  GDB_COMMAND: "whatis"

   function Info_Locals
     (Debugger : access Debugger_Root) return String is abstract;
   --  Return the command to be used to display local variables

   function Info_Args
     (Debugger : access Debugger_Root) return String is abstract;
   --  Return the command to be used to display the parameters of the current
   --  subprogram

   function Info_Registers
     (Debugger : access Debugger_Root) return String is abstract;
   --  Return the command to be used to display the value of registers

   function Value_Of
     (Debugger : access Debugger_Root;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String is abstract;
   --  Return the value of the entity.
   --  GDB_COMMAND: "print"
   --  JDB_COMMAND: "dump"

   function Print_Value_Cmd
     (Debugger : access Debugger_Root;
      Entity   : String) return String is abstract;
   --  Return the command to execute to get the value of the entity.
   --  GDB_COMMAND: "print " & Entity
   --  JDB_COMMAND: "dump " & Entity

   procedure Set_Variable
     (Debugger : access Debugger_Root;
      Var_Name : String;
      Value    : String);
   --  Set the value of a specific variable.
   --  Var_Name should contain any needed block information

   ------------------------------
   -- Source/Path manipulation --
   ------------------------------

   procedure Change_Directory
     (Debugger    : access Debugger_Root;
      Dir         : GNATCOLL.VFS.Virtual_File;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Change to directory Dir under a specified debugger session.
   --  See execution commands below for an explanation on the Mode parameter.

   procedure Found_File_Name
     (Debugger    : access Debugger_Root;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type);
   --  Search for a file name, line or address indication in Str.
   --  Str is a string output by the debugger, that might contain a reference
   --  to a specific file and line, that we want to display in the code editor
   --  window.
   --  Name is set to Null_Unbounded_String is no filename was found.
   --  Line is set to 0 is not found.
   --  Addr is set to Invalid_Address if none found.
   --
   --  Implementation Note: This could have been done by adding another output
   --  filter to the debugger, that would take care of parsing the output.
   --  However, since display a file requires multiple operations, it seemed
   --  better to do it in GVD.Process.Text_Output_Handler.
   --  Note that the last reference to a file or a line should be used, in case
   --  multiple references are found in Str.

   type Frame_Info_Type is
     (Location_Not_Found,
      Location_Found,
      No_Debug_Info);

   procedure Found_Frame_Info
     (Debugger    : access Debugger_Root;
      Str         : String;
      Frame       : out Unbounded_String;
      Message     : out Frame_Info_Type);
   --  Search for a callstack frame indication in Str.
   --  Message is set to No_Debug_Info if no frame info found, and Frame
   --  to Null_Unbounded_String.
   --  Otherwise Frame contains the string corresponding to the frame number.

   function Source_Files_List
     (Debugger : access Debugger_Root) return GNAT.Strings.String_List;
   --  Return the list of source files for the currently loaded executable.
   --  If the debugger can not return a list of specific sources, it should
   --  return an empty array.
   --  GDB_COMMAND: "info sources"

   function Find_File
     (Debugger : access Debugger_Root; File_Name : String) return String;
   --  Return the full path name for File_Name.
   --  If File_Name is not found, then it is returned as is.

   ------------------------
   -- Execution Commands --
   ------------------------

   --  Mode parameter:
   --  ==================
   --  In all the following subprograms, mode is used to output the command
   --  in the debugger command window and to add the command to the command
   --  history. If Internal or Hidden is passed for this parameter, the command
   --  is not shown in the command window associated with the debugger.
   --  Additionally, the command history is not updated for internal commands.

   procedure Set_Executable
     (Debugger   : access Debugger_Root;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      is abstract;
   --  Load an executable into the debugger.
   --  Note that this can have a different meaning with some languages like
   --  Java, where Executable should be the name of the main class.
   --  Raises Executable_Not_Found when Executable could not be loaded in
   --  the debugger.
   --  GDB_COMMAND: "file"

   function Get_Executable
     (Debugger : access Debugger_Root)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return the name of the executable currently debugged.

   procedure Load_Core_File
     (Debugger : access Debugger_Root;
      Core     : GNATCOLL.VFS.Virtual_File;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Load a core file into the debugger.
   --  GDB_COMMAND: "core"

   procedure Add_Symbols
     (Debugger : access Debugger_Root;
      Module   : GNATCOLL.VFS.Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Load symbols from a specified module into the debugger.
   --  GDB_COMMAND: "add-symbol-file"

   procedure Load_Executable
     (Debugger   : access Debugger_Root;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Load the given executable to the remote target, if any.
   --  When Executable is not specified, load the currently debugged
   --  executable instead.
   --  GDB_COMMAND: "load"

   procedure Run
     (Debugger  : access Debugger_Root;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Start the execution of the executable.
   --  Arguments is a string passed on the command line to run
   --  Note that this command does not wait for the prompt, and returns
   --  immediately.
   --  See above for details on Display.
   --  GDB_COMMAND: "run"

   procedure Start
     (Debugger : access Debugger_Root;
      Arguments : String := "";
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Start the execution of the executable and stop at the first user line.
   --  Arguments is a string passed on the command line to run
   --  The arguments must have been set by a call to Set_Arguments.
   --  See above for details on Display.
   --  GDB_COMMAND: "begin"

   procedure Attach_Process
     (Debugger : access Debugger_Root;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Attach a given process into the debugger.
   --  GDB_COMMAND: "attach"

   procedure Detach_Process
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Detach the current process from the debugger.
   --  GDB_COMMAND: "detach"

   procedure Kill_Process
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Kill the current process.
   --  GDB_COMMAND: "kill"

   procedure Step_Into
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Step program until it reaches a different source line.
   --  See above for details on Display.
   --  GDB_COMMAND: "step"

   procedure Step_Over
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Step program, proceeding over subroutines.
   --  See above for details on Display.
   --  GDB_COMMAND: "next"

   procedure Step_Into_Instruction
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Step program until it reaches a different assembly line
   --  See above for details on Display.
   --  GDB_COMMAND: "stepi"

   procedure Step_Over_Instruction
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Step program one assembly instruction, proceeding over subroutines.
   --  See above for details on Display.
   --  GDB_COMMAND: "nexti"

   procedure Continue
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Continue program after signal or breakpoint.
   --  See above for details on Display.
   --  GDB_COMMAND: "cont"

   procedure Interrupt (Debugger : access Debugger_Root) is abstract;
   --  Interrupt the debugger, or the debuggee if it is running.

   type Command_Category is
     (Load_Command,
      Context_Command,
      Execution_Command,
      Misc_Command);
   --  Kind of command:
   --    - Load: Changes the module(s) to debug (e.g. load file).
   --    - Context: command changes the context of the debugee, e.g. thread
   --               switching.
   --    - Execution: execution command for the debuggee, e.g. step, next.
   --    - Misc: None of the above.

   function Command_Kind
     (Debugger : access Debugger_Root;
      Command  : String) return Command_Category is abstract;
   --  Return the kind of command associated with Command

   function Breakpoints_Changed
     (Debugger : access Debugger_Root;
      Command  : String) return Boolean is abstract;
   --  Return True if the list of breakpoints has likely changed after
   --  Command has run.

   function Is_Started (Debugger : access Debugger_Root)
     return Boolean;
   --  Return True if the debuggee executable has been started.

   procedure Set_Is_Started
     (Debugger   : access Debugger_Root;
      Is_Started : Boolean);
   --  Set the Is_Started state for the debuggee executable

   ----------------------
   -- Stack Management --
   ----------------------

   procedure Stack_Down
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Select and print stack frame called by the current one.
   --  See above for details on Display.
   --  GDB_COMMAND: "down"

   procedure Stack_Up
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Select and print stack frame that called the current one.
   --  See above for details on Display.
   --  GDB_COMMAND: "up"

   procedure Stack_Frame
     (Debugger : access Debugger_Root;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Select and print the selected stack frame.
   --  The first frame is 1. It is up to the real debugger to convert to the
   --  appropriate Id when needed.
   --  See above for details on Display.
   --  GDB_COMMAND: "frame"

   procedure Finish
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Finish executing the current frame.
   --  See above for details on Display.
   --  GDB_COMMAND: "finish"

   ---------------
   -- Backtrace --
   ---------------

   type Backtrace_Subprogram_Parameter is record
      Value : GNAT.Strings.String_Access;
   end record;

   package Backtrace_Subprogram_Parameters_Vectors is
     new Ada.Containers.Vectors (Positive, Backtrace_Subprogram_Parameter);

   type Backtrace_Record is record
      Frame_Id        : Natural := 0;
      Address         : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      Subprogram      : GNAT.Strings.String_Access;
      Parameters      : Backtrace_Subprogram_Parameters_Vectors.Vector;
      File            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line            : Natural := 0;
   end record;

   package Backtrace_Vectors is
     new Ada.Containers.Vectors (Positive, Backtrace_Record);
   subtype Backtrace_Vector is Backtrace_Vectors.Vector;

   procedure Free (Bt : in out Backtrace_Vector);
   --  Free all the dynamic memory associated with each backtrace record.

   procedure Backtrace
     (Debugger : access Debugger_Root;
      Value    : out Backtrace_Vector) is abstract;
   --  Return the current backtrace.
   --  GDB_COMMAND: "bt"

   function Current_Frame
     (Debugger : access Debugger_Root)
      return Integer is abstract;
   --  Return the number of current frame. Should return -1 if no frames.

   procedure Configure_Backtrace
     (Self                 : not null access Debugger_Root;
      Show_Id              : Boolean := True;
      Show_PC              : Boolean := True;
      Show_Subprogram_Name : Boolean := True;
      Show_Parameters      : Boolean := True;
      Show_Location        : Boolean := True) is null;
   --  Configure which information will be displayed in the call stack view.
   --  By taking advantage of which info is not necessary, this might speed up
   --  the computation of the call stack in the debugger.
   --
   --  GDB_COMMAND: set print frame-arguments

   -------------------------
   -- Breakpoint Handling --
   -------------------------

   function Break_Subprogram
     (Debugger  : access Debugger_Root;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
      is abstract;
   --  Break at the beginning of a specific subprogram.
   --  If Temporary is True, then the breakpoint should be deleted
   --  automatically the first time it is hit.
   --  It returns the identifier associated with the newly created breakpoint.
   --  GDB_COMMAND: "break name" or "tbreak name" (or No_Breakpoint if the
   --  debugger is busy and the command was queued)

   function Break_Source
     (Debugger  : access Debugger_Root;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
      is abstract;
   --  Break at a specific source location.
   --  If Temporary is True, then the breakpoint should be deleted
   --  automatically the first time it is hit.
   --  It returns the identifier associated with the newly created breakpoint.
   --  GDB_COMMAND: "break file:line" or "tbreak file:line"

   function Break_Exception
     (Debugger  : access Debugger_Root;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
      is abstract;
   --  Break on an exception, if the debugger and the language recognize that
   --  feature.
   --  The breakpoint is set on a specific exception Name (or all exceptions
   --  if Name is "" or "all").
   --  The breakpoint is activated only for unhandled exceptions if Unhandled
   --  is True, or for all exceptions if False.
   --  Not all combinations are possible (for instance, Gdb in Ada mode can
   --  not break on a specific exception only when it is unhandled).
   --  GDB_COMMAND: "break exception"

   function Break_Address
     (Debugger   : access Debugger_Root;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
      is abstract;
   --  Set a breakpoint at a specific address.

   function Break_Regexp
     (Debugger   : access Debugger_Root;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
      is abstract;
   --  Set a breakpoint on all subprograms matching Regexp.
   --  This function is emulated when the debugger does not support it
   --  directly.

   procedure Enable_Breakpoint
     (Debugger : access Debugger_Root;
      Num      : GVD.Types.Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Enable or disable the breakpoint number Num.
   --  Num is always the number returned in the Num field of the
   --  Breakpoint_Data record by List_Breakpoints.

   procedure Remove_Breakpoint
     (Debugger : access Debugger_Root;
      Num      : GVD.Types.Breakpoint_Identifier;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Delete a breakpoint.
   --  Num is always the number returned in the Num field of the
   --  Breakpoint_Data record.

   procedure Remove_Breakpoint_At
     (Debugger : not null access Debugger_Root;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is abstract;
   --  Remove any breakpoint set at that location

   function To_File
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String)
     return GNATCOLL.VFS.Virtual_File;
   --  Convert from a file name read from the debugger to a Virtual_File.
   --  This takes into account the fact that program might have been
   --  compiled on another machine, with sources located elsewhere

   procedure List_Breakpoints
     (Debugger  : not null access Debugger_Root;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector) is abstract;
   --  Return the list of breakpoints set in the current session.

   function Get_Last_Breakpoint_Id
     (Debugger  : access Debugger_Root)
      return GVD.Types.Breakpoint_Identifier is abstract;
   --  Return the Id of the last created breakpoint.
   --  Some of the Break_* commands above might create several breakpoints, so
   --  it might not be accurate to rely on this function to get the list of
   --  all breakpoints created by a Break_* subprograms

   procedure Set_Breakpoint_Condition
     (Debugger  : access Debugger_Root;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Set the condition on which a breakpoint should be activated.

   procedure Set_Breakpoint_Command
     (Debugger : access Debugger_Root;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Set the commands to execute upon stopping at the breakpoint.
   --  One command per line in commands.

   procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Debugger_Root;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Set the number of times the breakpoint should be ignored before being
   --  activated.

   procedure Set_Scope_Action
     (Debugger : access Debugger_Root;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Set the scope/action of the breakpoint identified by Num:
   --  GDB_COMMAND: change-breakpoint-scope/change-breakpoint-action
   --  Set the default scope/action of a debugging session if Num = 0:
   --  GDB_COMMAND: set break-command-scope/set break-command-action

   -----------------
   -- Watchpoints --
   -----------------

   function Watch
     (Debugger  : access Debugger_Root;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
      is abstract;
   --  Set a watchpoint for the variable or memory location in Name.
   --  Trigger specifies that the watchpoint is activated by a read, write,
   --  or either to the memory location.  If a condition string is given,
   --  then program execution will stop only when it evaluates as true.
   --  GDB_COMMAND: "(watch|rwatch|awatch) <name> [if <condition>]"

   ----------------
   -- Exceptions --
   ----------------

   function List_Exceptions
     (Debugger : access Debugger_Root) return GVD.Types.Exception_Array;
   --  Return the list of exceptions defined in the current session.
   --  An empty array is returned no breakpoint can be set on exceptions (this
   --  is the default behavior).

   --------------------
   -- Thread Support --
   --------------------

   subtype Thread_Fields is Interfaces.C.size_t range 1 .. 20;
   --  This represents the maximum number of fields in a thread list output.

   type Thread_Information
     (Num_Fields : Thread_Fields := Thread_Fields'First) is
   record
      Information : chars_ptr_array (1 .. Num_Fields);
   end record;
   --  Represent the information of one thread.

   type Thread_Information_Array is
     array (Positive range <>) of Thread_Information;
   --  List of thread information.

   subtype PD_Information_Array is Thread_Information_Array;
   --  List of protection domains information

   procedure Free (Info : in out Thread_Information_Array);
   --  Free the dyamic memory associated with each element of the array.

   procedure Task_Switch
     (Debugger : access Debugger_Root;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Switch to a specified task.
   --  GDB_COMMAND: "task"

   procedure Thread_Switch
     (Debugger : access Debugger_Root;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Switch to a specified thread.
   --  GDB_COMMAND: "thread"

   procedure PD_Switch
     (Debugger : access Debugger_Root;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is null;
   --  Switch to a specified protection domain.
   --  GDB_COMMAND: "pd <pd_id>"

   procedure Info_Tasks
     (Debugger : access Debugger_Root;
      Info     : out Thread_Information_Array;
      Len      : out Natural);
   --  Return the current list of tasks.
   --  GDB_COMMAND: "info tasks"

   procedure Info_Threads
     (Debugger : access Debugger_Root;
      Info     : out Thread_Information_Array;
      Len      : out Natural);
   --  Return the current list of threads.
   --  GDB_COMMAND: "info threads"

   procedure Info_PD
     (Debugger : access Debugger_Root;
      Info     : out PD_Information_Array;
      Len      : out Natural);
   --  Return the current list of protection domains.
   --  GDB_COMMAND: "info pds"

   procedure Set_VxWorks_Version
     (Debugger : access Debugger_Root; Force : Boolean := False) is null;
   --  Determine the VxWorks version running on the target

   function VxWorks_Version
     (Debugger : access Debugger_Root)
     return GVD.Types.VxWorks_Version_Type;
   --  Retrieve the VxWorks version stored in the debugger record

   -------------------
   -- Assembly code --
   -------------------

   package Disassemble_Element_Vectors is new Ada.Containers.Vectors
     (Positive, GVD.Types.Disassemble_Element, "=" => GVD.Types."=");

   subtype Disassemble_Elements is Disassemble_Element_Vectors.Vector;

   procedure Get_Machine_Code
     (Debugger        : access Debugger_Root;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type;
      Code            : out Disassemble_Elements;
      Start_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address     : GVD.Types.Address_Type := GVD.Types.Invalid_Address)
   is abstract;
   --  Return the machine code (or assembly code) for a specific region.
   --  The region disassembled is Start_Address .. End_Address (where the
   --  meaning of address depends on the target (this can be a machine
   --  address or an offset in the JVM).
   --  If Start_Address is Invalid_Address, then the code for the current frame
   --  is returned.

   procedure Get_Machine_Code
     (Debugger : access Debugger_Root;
      File     : String;
      From     : Natural;
      To       : Natural;
      Code     : out Disassemble_Elements)
   is abstract;
   --  Disassemble whole subpgogram

   procedure Get_Line_Address
     (Debugger        : access Debugger_Root;
      Line            : Natural;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type) is abstract;
   --  Return the range of addresses for a given source line.
   --  See Get_Machine_Code for an explanation of the parameters.

   ---------------
   -- Registers --
   ---------------

   function Get_Register_Names
     (Debugger : access Debugger_Root)
      return GVD.Types.Strings_Vectors.Vector is abstract;
   --  Return names of registers

   function Get_Registers_Values
     (Debugger : access Debugger_Root;
      Names    : GVD.Types.Strings_Vectors.Vector;
      Format   : GVD.Types.Registers_Format)
      return GVD.Types.Strings_Vectors.Vector is abstract;

   procedure Set_Register
     (Debugger : access Debugger_Root;
      Name     : String;
      Value    : String) is abstract;

   ----------------
   -- Exceptions --
   ----------------

   Unknown_Command : exception;
   --  Raised when a command is not recognized either by the debugger, or for
   --  the currently activate language.

   Spawn_Error : exception;
   --  Raised when the debugger could not be spawned.

   Executable_Not_Found : exception;
   --  Raised when the parameter to Set_Executable could not be loaded.

   -----------------------
   -- Memory operations --
   -----------------------

   type Memory_Dump_Item is record
      Label : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Element of memody dump. Value should be a hexadecimal string with no
   --  separator. Label is optional symbol corresponding to first byte of
   --  Value.

   type Memory_Dump is array (Positive range <>) of Memory_Dump_Item;

   type Memory_Dump_Access is access all Memory_Dump;

   function Get_Memory
     (Debugger : access Debugger_Root;
      Size     : Integer;
      Address  : String) return Memory_Dump_Access is abstract;
   --  Return the contents of the byte at a given address. The output should
   --  have representation for Size bytes.
   --  Address is "0x" followed by an hexadecimal number.

   procedure Put_Memory_Byte
     (Debugger : access Debugger_Root;
      Address  : String;
      Byte     : String) is abstract;
   --  Write the contents of one byte into the memory.
   --  Address is "0x" followed by an hexadecimal number.
   --  Byte is two hexadicimal digits.

   function Get_Variable_Address
     (Debugger  : access Debugger_Root;
      Variable  : String) return String is abstract;
   --  Returns the starting address for a given variable.
   --  The returned adress should be "0x" followed by an hexadecimal number.
   --  Alternatively, it returns "" if no such variable is found.

   type Endian_Type is (Unknown_Endian, Little_Endian, Big_Endian);

   function Get_Endian_Type
     (Debugger : access Debugger_Root) return Endian_Type is abstract;
   --  Get the endianness of the target.

   -----------------------------
   -- Command Line operations --
   -----------------------------

   function Complete
     (Debugger  : access Debugger_Root;
      Beginning : String) return GNAT.Strings.String_List is abstract;
   --  Return a list of commands recognized by the debugger that begin with
   --  Beginning.
   --  Note that the caller is responsible for freeing the memory allocated
   --  in the returned String_Array.

   ------------------------
   -- Process operations --
   ------------------------

   procedure Open_Processes (Debugger : access Debugger_Root);
   --  Initialize a connection to the host and target associated with
   --  Debugger in order to retrieve process information.

   procedure Next_Process
     (Debugger : access Debugger_Root;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean);
   --  Return information concerning the next process.
   --  Success is set to True if there is a remaining process, false otherwise.

   procedure Close_Processes (Debugger : access Debugger_Root);
   --  Close the connection established by Open_Processes.
   --  Note that this function must be called before calling Open_Processes
   --  again. In particular it ensures that all the memory associated with
   --  the current connection is freed.

   function Support_TTY (Debugger : access Debugger_Root) return Boolean;
   --  Return True if the given debugger supports a Set_TTY command.

   procedure Set_TTY
     (Debugger : access Debugger_Root; TTY : String) is abstract;
   --  If supported (see Support_TTY above), set the terminal of the program
   --  debugged to TTY (e.g "/dev/pts/2").
   --  If not supported, raise Unknown_Command.

   procedure Filter_Output
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String);
   --  Filter Str from any e.g. internal strings, return filtered output
   --  in Result. Default procedure returns Str.

   function Is_Quit_Command
     (Debugger : access Debugger_Root;
      Command : String) return Boolean;
   --  Return true if Command will close the debugger.
   --  Default implementation always returns False.
   --  Note that Command is assumed to be all lower case, the caller is
   --  responsible for ensuring that.

   function Continuation_Line
     (Debugger : access Debugger_Root) return Boolean;
   --  Whether the debugger is currently handling a multiple line command.

   function Separate_Execution_Window
     (Debugger : access Debugger_Root) return Boolean;
   --  Whether the debugger has a separate execution window.

   function Get_Remote_Target
     (Debugger : access Debugger_Root) return String;
   --  Return the debugger's current remote target.
   --  If no remote target has been specified yet, return an empty string.

   function Get_Remote_Protocol
     (Debugger : access Debugger_Root) return String;
   --  Return the debugger's current remote protocol.
   --  If no remote protocol has been specified yet, return an empty string.

   function Get_Kernel
     (Debugger : access Debugger_Root'Class)
      return GPS.Kernel.Kernel_Handle;
   --  Return the kernel

   Command_Intercepted : constant String := "----";
   --  See the documentation of the Debugger_Command_Action_Hook

private
   type Command_Record;
   type Command_Access is access Command_Record;
   type Command_Record is record
      Cmd             : GNAT.Strings.String_Access;
      Empty_Buffer    : Boolean;
      Mode            : GVD.Types.Command_Type;
      Next            : Command_Access;
   end record;

   package Language_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Language.Language_Access, Language."=");

   type Debugger_State is
     (Idle,              --  Debugger is waiting for the next command
      Sync_Wait,         --  Command sent, waiting for result before returning
      Async_Wait         --  Command sent, waiting for result without blocking
     );

   type Debugger_Root is abstract tagged record
      Kernel       : Kernel_Handle;
      Process      : Process_Proxies.Process_Proxy_Access := null;

      Languages    : Language_Lists.List;
      --  The list of languages in use for this debugger. New elements are
      --  added to the list when the current language changes. We reuse
      --  elements from the list when going back to a language already seen so
      --  that we can initialize internal data for these languages based on
      --  dynamically queried debugger features, without doing so every time
      --  the language changes.

      The_Language : Language_Lists.Cursor := Language_Lists.No_Element;
      --  The current language

      Is_Started : Boolean := False;
      --  True when the debugger session has been started (ie the execution
      --  of the debuggee has started, and the user can now use commands like
      --  Next, Step, ...)

      State : Debugger_State := Idle;
      --  State of the debugger and its debuggee.
      --  See Debugger_State for more details.
      --  ??? Consider merging Is_Started and State

      Command_Queue : Command_Access := null;
      --  The list of commands to be processed after the next call to wait.

      Continuation_Line : Boolean := False;
      --  Whether the debugger is currently handling a multiple line command.

      Remote_Target   : GNAT.Strings.String_Access;
      Remote_Protocol : GNAT.Strings.String_Access;

      Handle : GVD.Proc_Utils.Process_Handle;
      --  Handle used to implement Open/Next/Close_Process.

      Execution_Window : Boolean := False;
   end record;

end Debugger;
