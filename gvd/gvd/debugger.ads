
with Generic_Values;
with GNAT.Expect;
with Unchecked_Deallocation;

package Debugger is

   type Debugger_Root is abstract tagged private;
   --  The general base class for all debuggers.
   --  Each debugger should extend this base class.

   function Parse_Type (Debugger : Debugger_Root;
                        Entity   : String)
                       return Generic_Values.Generic_Type_Access
      is abstract;
   --  Parse the type definition for Entity, and return a 
   --  tree as explained in Generic_Values.

   procedure Wait_Prompt (Debugger : Debugger_Root) is abstract;
   --  Wait for the prompt.
   
   function Type_Of (Debugger : Debugger_Root;
                     Entity : String)
                    return String
      is abstract;
   --  Return the type of the entity.
   --  An empty string is returned if the entity is not defined in the
   --  current context.
   --  GDB_COMMAND: "ptype"

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
   
   function Backtrace (Debugger : Debugger_Root) return String is abstract;
   --  Return the current backtrace.
   
   Unknown_Command : exception;
   --  Raised when a command is not recognized either by the debugger, or for
   --  the currently activate language.
   
private
   
   type Pipes_Id_Access is access all GNAT.Expect.Pipes_Id;
   procedure Free is new Unchecked_Deallocation 
     (GNAT.Expect.Pipes_Id, Pipes_Id_Access);
   
   type Debugger_Root is abstract tagged record
      Process : Pipes_Id_Access := null;
   end record;
end Debugger;
