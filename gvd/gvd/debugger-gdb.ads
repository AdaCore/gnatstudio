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

--  This is the implementation of Debugger for the GNU Debugger (Gdb).
--  See debugger.ads for complete documentation on this package.

with Debugger;
with Generic_Values;

package Debugger.Gdb is

   type Gdb_Debugger is new Debugger.Debugger_Root with private;

   procedure Spawn (Debugger       : access Gdb_Debugger;
                    Remote_Machine : String := "");
   procedure Initialize (Debugger : access Gdb_Debugger);

   procedure Close (Debugger : in out Gdb_Debugger);

   function Parse_Type
     (Debugger  : Gdb_Debugger;
      Entity    : String) return Generic_Values.Generic_Type_Access;

   procedure Parse_Value
     (Debugger  : Gdb_Debugger;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access);

   procedure Wait_Prompt (Debugger : Gdb_Debugger);

   function Type_Of
     (Debugger : Gdb_Debugger;
      Entity : String) return String;

   function Value_Of
     (Debugger : Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String;

   procedure Set_Executable
     (Debugger : Gdb_Debugger;
      Executable : String);

   procedure Run (Debugger : Gdb_Debugger);

   procedure Start (Debugger : Gdb_Debugger);

   procedure Step_Into (Debugger : Gdb_Debugger);

   procedure Step_Over (Debugger : Gdb_Debugger);

   procedure Break_Exception
     (Debugger  : Gdb_Debugger;
      Name      : String  := "";
      Unhandled : Boolean := False);

   procedure Break_Subprogram
     (Debugger : Gdb_Debugger;
      Name     : String);

   procedure Finish (Debugger : Gdb_Debugger);

   function Backtrace (Debugger : Gdb_Debugger) return String;

   function Line_Contains_Code
     (Debugger : Gdb_Debugger;
      File     : String;
      Line     : Positive) return Boolean;

private
   type Gdb_Debugger is new Debugger.Debugger_Root with null record;
end Debugger.Gdb;
