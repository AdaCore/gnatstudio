-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Language;
with Generic_Values;
with GNAT.OS_Lib;
with Process_Proxies;
with GNAT.Regpat;
with Gtk.Window;

package Debugger is

   type Debugger_Root is abstract tagged private;
   --  The general base class for all debuggers.
   --  Each debugger should extend this base class.

   type Debugger_Access is access all Debugger_Root'Class;

   type Debugger_Type is
     (Gdb_Type,
      Dbx_Type,
      Xdb_Type,
      Jdb_Type,
      Pydb_Type,
      Perl_Type,
      Ladebug_Type);
   --  Type of debugger handled.
   --  Beware that some debuggers might not be available.

   procedure Spawn
     (Debugger        : access Debugger_Root;
      Executable      : String;
      Arguments       : GNAT.OS_Lib.Argument_List;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") is abstract;
   --  Spawn the external process.
   --  Initialize should be called afterwards, but this is done in two
   --  separate steps so that it is possible to set filters.
   --
   --  Executable is the name of the module to debug.
   --
   --  Arguments are additional arguments to pass to the debugger.
   --
   --  Proxy is assigned to the debugger, after its underlying process has
   --  been created.
   --
   --  Window is the main window that is associated with this debugger.
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
      Arguments      : GNAT.OS_Lib.Argument_List;
      Debugger_Name  : String;
      Proxy          : Process_Proxies.Process_Proxy_Access;
      Remote_Machine : String := "");
   --  Convenience function to start a debugger.
   --  This command modifies the argument list so that the debugger can also
   --  be run on a remote machine.
   --  This is provided as a support for the implementation of the primitive
   --  subprogram Spawn, and should work with most debuggers.
   --
   --  Raises Spawn_Error if the debugger could not be spawned.

   procedure Initialize (Debugger : access Debugger_Root) is abstract;
   --  Initialize the debugger.
   --  Spawn must have been called first.

   function Highlighting_Pattern (Debugger : access Debugger_Root)
                                 return GNAT.Regpat.Pattern_Matcher
      is abstract;
   --  Return a regular expression that should match everything that should
   --  be highlighted in the debugger text window.

   procedure Close (Debugger : access Debugger_Root) is abstract;
   --  Terminates the external process.

   function Get_Process
     (Debugger : access Debugger_Root)
     return Process_Proxies.Process_Proxy_Access;
   --  Return the process descriptor associated with Debugger.

   procedure Set_Language
     (Debugger     : access Debugger_Root;
      The_Language : Language.Language_Access);
   --  Set the language associated with a debugger.
   --  Note that this procedure will free the previous language associated
   --  with Debugger, if any.

   function Get_Language
     (Debugger : access Debugger_Root) return Language.Language_Access;
   --  Return the current language associated with a debugger.

   function Parse_Type
     (Debugger : access Debugger_Root'Class;
      Entity   : String) return Generic_Values.Generic_Type_Access;
   --  Parse the type definition for Entity, and return a
   --  tree as explained in Generic_Values.

   procedure Parse_Value
     (Debugger    : access Debugger_Root'Class;
      Entity      : String;
      Value       : in out Generic_Values.Generic_Type_Access;
      Value_Found : out Boolean);
   --  Parse the value of Entity.
   --  Value should contain the result of Parse_Type when this procedure is
   --  called, and it is completed to reflect the new value.
   --  Value_Found is set to True only if a valid Value could be found for the
   --  variable.

   function Get_Uniq_Id
     (Debugger : access Debugger_Root;
      Entity   : String)
     return String;
   --  Return a uniq ID for Entity.
   --  In most cases, this will be the address of the variable. However, some
   --  languages do not have addresses (Java), but since they do not allow
   --  overloading, returning the name might be enough (This is the default)
   --
   --  This ID is used to detect aliases in the canvas display.
   --  The return result be the value of access types (ie types that can be
   --  dereferenced).

   procedure Wait_Prompt (Debugger : access Debugger_Root) is abstract;
   --  Wait for the prompt.

   procedure Display_Prompt (Debugger : access Debugger_Root) is abstract;
   --  Send a command to the debugger, so that the prompt is displayed
   --  again in the debugger window. This is used after internal commands like
   --  "graph print", to indicate that the command has finished executing.

   procedure Found_File_Name
     (Debugger    : access Debugger_Root;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural);
   --  Search for a file name indication in Str.
   --  Str is a string output by the debugger, that might contain a reference
   --  to a specific file and line, that we want to display in the code editor
   --  window.
   --  On output, the name of the file is Str (Name_First .. Name_Last), and
   --  the line is Line.
   --  Set Name_First to 0 if no file name was found, and set Line to 0 if
   --  no line was found.
   --  First and Last point to the slice of Str that should be stripped from
   --  the output. They are reset to zero when no slice should be stripped.
   --  Note that the last reference to a file or a line should be used, in case
   --  multiple references are found in Str.
   --
   --  Implementation Note: This could have been done by adding another output
   --  filter to the debugger, that would take care of parsing the output.
   --  However, since display a file requires multiple operations, it seemed
   --  better to do it in Odd.Process.Text_Output_Handler.

   function Type_Of
     (Debugger : access Debugger_Root;
      Entity   : String) return String is abstract;
   --  Return the type of the entity.
   --  An empty string is returned if the entity is not defined in the
   --  current context.
   --  GDB_COMMAND: "ptype"

   type Value_Format is (Decimal, Binary, Hexadecimal, Octal);

   function Value_Of
     (Debugger : access Debugger_Root;
      Entity   : String;
      Format   : Value_Format := Decimal) return String is abstract;
   --  Return the value of the entity.
   --  GDB_COMMAND: "print"

   procedure Set_Executable
     (Debugger : access Debugger_Root; Executable : String) is abstract;
   --  Load an executable into the debugger.
   --  Note that this can have a different meaning with some languages like
   --  Java, where Executable should be the name of the main class.
   --  GDB_COMMAND: "file"

   ------------------------
   -- Execution Commands --
   ------------------------

   --  Window parameter:
   --  =================
   --  In all the following subprograms, Window is the main debug window, in
   --  which the debugger is displayed.  It is used to output the command in
   --  the debugger command window. If null is passed for this parameter, the
   --  command is not shown in the command window.

   procedure Run (Debugger : access Debugger_Root;
                  Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Start the execution of the executable.
   --  The arguments must have been set by a call to Set_Arguments.
   --  Note that this command does not wait for the prompt, and returns
   --  immediately.
   --  See above for details on Window.
   --  GDB_COMMAND: "run"

   procedure Start (Debugger : access Debugger_Root;
                    Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Start the execution of the executable and stop at the first user line.
   --  The arguments must have been set by a call to Set_Arguments.
   --  See above for details on Window.
   --  GDB_COMMAND: "begin"

   procedure Step_Into (Debugger : access Debugger_Root;
                        Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Step program until it reaches a different source line.
   --  See above for details on Window.
   --  GDB_COMMAND: "step"

   procedure Step_Over (Debugger : access Debugger_Root;
                        Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Step program, proceeding over subroutines.
   --  See above for details on Window.
   --  GDB_COMMAND: "next"

   procedure Continue (Debugger : access Debugger_Root;
                       Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Continue program after signal or breakpoint.
   --  See above for details on Window.
   --  GDB_COMMAND: "next"

   procedure Interrupt (Debugger : access Debugger_Root) is abstract;
   --  Interrupt the debugger, or the debuggee if it is running.

   ----------------------
   -- Stack Management --
   ----------------------

   procedure Stack_Down
     (Debugger : access Debugger_Root;
      Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Select and print stack frame called by the current one.
   --  See above for details on Window.
   --  GDB_COMMAND: "down"

   procedure Stack_Up
     (Debugger : access Debugger_Root;
      Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Select and print stack frame that called the current one.
   --  See above for details on Window.
   --  GDB_COMMAND: "up"

   procedure Stack_Frame
     (Debugger : access Debugger_Root;
      Frame    : Positive;
      Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Select and print the selected stack frame.
   --  The first frame is 1. It is up to the real debugger to convert to the
   --  appropriate Id when needed.
   --  See above for details on Window.
   --  GDB_COMMAND: "frame"

   procedure Finish
     (Debugger : access Debugger_Root;
      Window   : Gtk.Window.Gtk_Window := null) is abstract;
   --  Finish executing the current frame.
   --  See above for details on Window.
   --  GDB_COMMAND: "finish"

   type Backtrace_Record is record
      Frame_Id        : Natural;
      Program_Counter : Generic_Values.String_Access;
      Subprogram      : Generic_Values.String_Access;
      Source_Location : Generic_Values.String_Access;
   end record;

   type Backtrace_Array is array (Positive range <>) of Backtrace_Record;

   procedure Free (Bt : in out Backtrace_Array);
   --  Free all the dynamic memory associated with each backtrace record.

   procedure Backtrace
     (Debugger : access Debugger_Root;
      Value    : out Backtrace_Array;
      Len      : out Natural) is abstract;
   --  Return the current backtrace.
   --  GDB_COMMAND: "bt"

   -------------------------
   -- Breakpoint Handling --
   -------------------------

   procedure Break_Subprogram
     (Debugger : access Debugger_Root; Name : String) is abstract;
   --  Break at the beginning of a specific subprogram.
   --  GDB_COMMAND: "break name"

   procedure Break_Source
     (Debugger : access Debugger_Root;
      File     : String;
      Line     : Positive) is abstract;
   --  Break at a specific source location.
   --  GDB_COMMAND: "break file:line"

   procedure Break_Exception
     (Debugger  : access Debugger_Root;
      Name      : String  := "";
      Unhandled : Boolean := False) is abstract;
   --  Break on an exception, if the debugger and the language recognize that
   --  feature.
   --  The breakpoint is set on a specific exception Name (or all exceptions
   --  if Name is "").
   --  The breakpoint is activated only for unhandled exceptions if Unhandled
   --  is True, or for all exceptions if False.
   --  Note all combinations are possible (for instance, Gdb in Ada mode can
   --  not break on a specific exception only when it is unhandled).
   --  GDB_COMMAND: "break exception"

   --------------------
   -- Thread Support --
   --------------------

   procedure Thread_Switch
     (Debugger : access Debugger_Root'Class;
      Thread   : Natural);
   --  Switch to a specified thread.
   --  GDB_COMMAND: "thread" or "task"

   function Info_Threads
     (Debugger  : access Debugger_Root)
      return Language.Thread_Information_Array is abstract;
   --  Return the current list of threads.
   --  GDB_COMMAND: "info threads" or "info tasks"

   -----------------------------
   -- Source Related Commands --
   -----------------------------

   function Line_Contains_Code
     (Debugger : access Debugger_Root;
      File     : String;
      Line     : Positive) return Boolean is abstract;
   --  Indicate whether a given file and line number contain executable code.

   Unknown_Command : exception;
   --  Raised when a command is not recognized either by the debugger, or for
   --  the currently activate language.

   Spawn_Error : exception;
   --  Raised when the debugger could not be spawned.

private

   type Debugger_Root is abstract tagged record
      Process      : Process_Proxies.Process_Proxy_Access := null;
      The_Language : Language.Language_Access;
   end record;
end Debugger;
