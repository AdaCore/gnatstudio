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

with Debugger;
with Generic_Values;

package Debugger.Gdb is

   --  Note: the parser functions should be put in a child package instead.

   --  Also, they should be dispatching on the language, if we create such
   --  a tagged type, instead of making explicit checks.

   --  The Gdb_Debugger class should directly know how to communicate with the
   --  external process, so that it can make the requests itself.

   type Gdb_Debugger is new Debugger.Debugger_Root with private;

   function Parse_Type
     (Debugger  : Gdb_Debugger;
      Entity    : String) return Generic_Values.Generic_Type_Access;
   --  Parse the definition of type for Entity.

   procedure Parse_Value
     (Debugger  : Gdb_Debugger;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access);
   --  Parse the value for Entity.

   function Type_Of (Debugger : Gdb_Debugger;
                     Entity : String)
                    return String;
   --  Return the type of Entity in the current context.

   function Value_Of (Debugger : Gdb_Debugger;
                      Entity   : String;
                      Format   : Value_Format := Decimal)
                     return String;
   --  Print the value of the entity.

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

   procedure Break_Subprogram (Debugger : Gdb_Debugger;
                               Name     : String);

   procedure Finish (Debugger : Gdb_Debugger);

private
   type Gdb_Debugger is new Debugger.Debugger_Root with null record;
end Debugger.Gdb;
