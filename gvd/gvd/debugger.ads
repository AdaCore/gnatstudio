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

package Debugger is

   type Debugger_Root is abstract tagged private;
   --  The general base class for all debuggers.
   --  Each debugger should extend this base class.

   type Debugger_Access is access all Debugger_Root'Class;

   procedure Spawn
     (Debugger       : access Debugger_Root;
      Arguments      : GNAT.OS_Lib.Argument_List;
      Proxy          : Process_Proxies.Process_Proxy_Access;
      Remote_Machine : String := "") is abstract;
   --  Spawn the external process.
   --  Initialize should be called afterwards, but this is done in two
   --  separate steps so that it is possible to set filters.
   --
   --  If Remote_Machine is different from the empty string, the debugger
   --  is spawned on the remote machine.
   --
   --  Proxy is assigned to the debugger, after its underlying process has
   --  been created.

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

   function Get_Language
     (Debugger : access Debugger_Root) return Language.Language_Access;
   --  Return the current language associated with a debugger.

   function Parse_Type
     (Debugger : access Debugger_Root'Class;
      Entity   : String) return Generic_Values.Generic_Type_Access;
   --  Parse the type definition for Entity, and return a
   --  tree as explained in Generic_Values.

   procedure Parse_Value
     (Debugger  : access Debugger_Root'Class;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access);
   --  Parse the value of Entity.
   --  Value should contain the result of Parse_Type when this procedure is
   --  called, and it is completed to reflect the new value.

   procedure Wait_Prompt (Debugger : access Debugger_Root) is abstract;
   --  Wait for the prompt.

   function Type_Of
     (Debugger : access Debugger_Root;
      Entity   : String) return String is abstract;
   --  Return the type of the entity.
   --  An empty string is returned if the entity is not defined in the
   --  current context.
   --  GDB_COMMAND: "ptype"

   type Value_Format is (Decimal,
                         Binary,
                         Hexadecimal,
                         Octal);

   function Value_Of (Debugger : access Debugger_Root;
                      Entity   : String;
                      Format   : Value_Format := Decimal)
                     return String
      is abstract;
   --  Return the value of the entity.
   --  GDB_COMMAND: "print"

   procedure Set_Executable (Debugger : access Debugger_Root;
                             Executable : String)
      is abstract;
   --  Load an executable into the debugger.
   --  Note that this can have a different meaning with some languages like
   --  Java, where Executable should be the name of the main class.
   --  GDB_COMMAND: "file"

   procedure Run (Debugger : access Debugger_Root) is abstract;
   --  Start the execution of the executable.
   --  The arguments must have been set by a call to Set_Arguments.
   --  Note that this command does not wait for the prompt, and returns
   --  immediately.
   --  GDB_COMMAND: "run"

   procedure Start (Debugger : access Debugger_Root) is abstract;
   --  Start the execution of the executable and stop at the first user line.
   --  The arguments must have been set by a call to Set_Arguments.
   --  GDB_COMMAND: "begin"

   procedure Step_Into (Debugger : access Debugger_Root) is abstract;
   --  Step program until it reaches a different source line.
   --  GDB_COMMAND: "step"

   procedure Step_Over (Debugger : access Debugger_Root) is abstract;
   --  Step program, proceeding over subroutines.
   --  GDB_COMMAND: "next"

   procedure Break_Exception (Debugger  : access Debugger_Root;
                              Name      : String  := "";
                              Unhandled : Boolean := False)
      is abstract;
   --  Break on an exception, if the debugger and the language recognize that
   --  feature.
   --  The breakpoint is set on a specific exception Name (or all exceptions
   --  if Name is "").
   --  The breakpoint is activated only for unhandled exceptions if Unhandled
   --  is True, or for all exceptions if False.
   --  Note all combinations are possible (for instance, Gdb in Ada mode can
   --  not break on a specific exception only when it is unhandled).
   --  GDB_COMMAND: "break exception"

   procedure Break_Subprogram (Debugger : access Debugger_Root;
                               Name     : String)
      is abstract;
   --  Break at the beginning of a specific subprogram.
   --  GDB_COMMAND: "break"

   procedure Finish (Debugger : access Debugger_Root) is abstract;
   --  Finish executing the current frame.
   --  GDB_COMMAND: "finish"

   function Backtrace (Debugger : access Debugger_Root) return String
      is abstract;
   --  Return the current backtrace.

   function Line_Contains_Code
     (Debugger : access Debugger_Root;
      File     : String;
      Line     : Positive) return Boolean is abstract;
   --  Indicate whether a given file and line number contain executable code.

   Unknown_Command : exception;
   --  Raised when a command is not recognized either by the debugger, or for
   --  the currently activate language.

private

   type Debugger_Root is abstract tagged record
      Process      : Process_Proxies.Process_Proxy_Access := null;
      The_Language : Language.Language_Access;
   end record;
end Debugger;
