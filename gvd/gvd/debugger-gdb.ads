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

with GNAT.OS_Lib;
with Debugger;
with GNAT.Regpat;

package Debugger.Gdb is

   type Gdb_Debugger is new Debugger.Debugger_Root with private;

   procedure Spawn (Debugger       : access Gdb_Debugger;
                    Arguments      : GNAT.OS_Lib.Argument_List;
                    Proxy          : Process_Proxies.Process_Proxy_Access;
                    Remote_Machine : String := "");
   procedure Initialize (Debugger : access Gdb_Debugger);

   procedure Close (Debugger : access Gdb_Debugger);

   procedure Wait_Prompt (Debugger : access Gdb_Debugger);

   function Highlighting_Pattern (Debugger : access Gdb_Debugger)
                                 return GNAT.Regpat.Pattern_Matcher;

   function Type_Of
     (Debugger : access Gdb_Debugger;
      Entity : String) return String;

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String;

   procedure Set_Executable
     (Debugger : access Gdb_Debugger;
      Executable : String);

   procedure Run (Debugger : access Gdb_Debugger);

   procedure Start (Debugger : access Gdb_Debugger);

   procedure Step_Into (Debugger : access Gdb_Debugger);

   procedure Step_Over (Debugger : access Gdb_Debugger);

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Unhandled : Boolean := False);

   procedure Break_Subprogram
     (Debugger : access Gdb_Debugger;
      Name     : String);

   procedure Finish (Debugger : access Gdb_Debugger);

   function Backtrace (Debugger : access Gdb_Debugger) return String;

   function Line_Contains_Code
     (Debugger : access Gdb_Debugger;
      File     : String;
      Line     : Positive) return Boolean;

private
   type Gdb_Debugger is new Debugger.Debugger_Root with null record;
end Debugger.Gdb;
