
with Debugger;
with Generic_Values;

package Debugger.Gdb is

   --  Note: the parser functions should be put in a child package instead.

   --  Also, they should be dispatching on the language, if we create such
   --  a tagged type, instead of making explicit checks.

   --  The Gdb_Debugger class should directly know how to communicate with the
   --  external process, so that it can make the requests itself.


   type Gdb_Debugger is new Debugger.Debugger_Root with private;

   function Parse_Type (Debugger  : Gdb_Debugger;
                        Entity    : String)
                       return Generic_Values.Generic_Type_Access;
   --  Parse the definition of type for Entity.

   function Type_Of (Debugger : Gdb_Debugger;
                     Entity : String)
                    return String;
   --  Return the type of Entity in the current context.

   procedure Set_Executable (Debugger : Gdb_Debugger;
                             Executable : String);
   --  Load the executable.

   procedure Initialize (Debugger : in out Gdb_Debugger);
   --  Spawn the external process, and initializes gdb.

   procedure Close (Debugger : in out Gdb_Debugger);
   --  Terminates the external process.

   procedure Wait_Prompt (Debugger : Gdb_Debugger);

   procedure Run (Debugger : Gdb_Debugger);

   procedure Break_Exception (Debugger  : Gdb_Debugger;
                              Name      : String  := "";
                              Unhandled : Boolean := False);

   function Backtrace (Debugger : Gdb_Debugger) return String;


private
   type Gdb_Debugger is new Debugger.Debugger_Root with null record;
end Debugger.Gdb;
