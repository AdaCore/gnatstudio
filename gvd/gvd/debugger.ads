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
with GNAT.Expect;
with Unchecked_Deallocation;

package Debugger is

   type Debugger_Root is abstract tagged private;
   --  The general base class for all debuggers.
   --  Each debugger should extend this base class.

   type Debugger_Access is access all Debugger_Root'Class;

   function Get_Process
     (Debugger : Debugger_Root) return GNAT.Expect.Pipes_Id_Access;
   --  Return the process descriptor associated with Debugger.

   procedure Set_Language
     (Debugger     : out Debugger_Root;
      The_Language : Language.Language_Access);
   --  Set the language associated with a debugger.

   function Get_Language
     (Debugger : Debugger_Root) return Language.Language_Access;
   --  Return the current language associated with a debugger.

   function Parse_Type (Debugger : Debugger_Root;
                        Entity   : String)
                       return Generic_Values.Generic_Type_Access
      is abstract;
   --  Parse the type definition for Entity, and return a
   --  tree as explained in Generic_Values.

   procedure Parse_Value
     (Debugger  : Debugger_Root;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access)
      is abstract;
   --  Parse the value of Entity.
   --  Value should contain the result of Parse_Type when this procedure is
   --  called, and it is completed to reflect the new value.

   procedure Wait_Prompt (Debugger : Debugger_Root) is abstract;
   --  Wait for the prompt.

   function Type_Of (Debugger : Debugger_Root;
                     Entity   : String)
                    return String
      is abstract;
   --  Return the type of the entity.
   --  An empty string is returned if the entity is not defined in the
   --  current context.
   --  GDB_COMMAND: "ptype"

   type Value_Format is (Decimal,
                         Binary,
                         Hexadecimal,
                         Octal);

   function Value_Of (Debugger : Debugger_Root;
                      Entity   : String;
                      Format   : Value_Format := Decimal)
                     return String
      is abstract;
   --  Return the value of the entity.
   --  GDB_COMMAND: "print"

   procedure Set_Executable (Debugger : Debugger_Root;
                             Executable : String)
      is abstract;
   --  Load an executable into the debugger.
   --  Note that this can have a different meaning with some languages like
   --  Java, where Executable should be the name of the main class.
   --  GDB_COMMAND: "file"

   procedure Run (Debugger : Debugger_Root) is abstract;
   --  Start the execution of the executable.
   --  The arguments must have been set by a call to Set_Arguments
   --  Note that this command does not wait for the prompt, and returns
   --  immediately.
   --  GDB_COMMAND: "run"

   procedure Break_Exception (Debugger  : Debugger_Root;
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

   procedure Break_Subprogram (Debugger : Debugger_Root;
                               Name     : String)
      is abstract;
   --  Break at the beginning of a specific subprogram.
   --  GDB_COMMAND: "break"

   procedure Finish (Debugger : Debugger_Root) is abstract;
   --  Finish executing the current frame.
   --  GDB_COMMAND: "finish"

   function Backtrace (Debugger : Debugger_Root) return String is abstract;
   --  Return the current backtrace.

   Unknown_Command : exception;
   --  Raised when a command is not recognized either by the debugger, or for
   --  the currently activate language.

private

   procedure Free is new Unchecked_Deallocation
     (GNAT.Expect.Pipes_Id, GNAT.Expect.Pipes_Id_Access);

   type Debugger_Root is abstract tagged record
      Process      : GNAT.Expect.Pipes_Id_Access := null;
      The_Language : Language.Language_Access;
   end record;
end Debugger;
