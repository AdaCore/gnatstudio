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

   procedure Spawn
     (Debugger        : access Gdb_Debugger;
      Executable      : String;
      Arguments       : GNAT.OS_Lib.Argument_List;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "");

   procedure Initialize (Debugger : access Gdb_Debugger);

   procedure Close (Debugger : access Gdb_Debugger);

   procedure Wait_Prompt (Debugger : access Gdb_Debugger);

   function Highlighting_Pattern
     (Debugger : access Gdb_Debugger) return GNAT.Regpat.Pattern_Matcher;

   procedure Display_Prompt (Debugger : access Gdb_Debugger);

   procedure Found_File_Name
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural);

   function Type_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String;

   function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String)
     return String;

   procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : String);

   procedure Run (Debugger : access Gdb_Debugger);

   procedure Start (Debugger : access Gdb_Debugger);

   procedure Step_Into (Debugger : access Gdb_Debugger);

   procedure Step_Over (Debugger : access Gdb_Debugger);

   procedure Continue (Debugger : access Gdb_Debugger);

   procedure Interrupt (Debugger : access Gdb_Debugger);

   procedure Stack_Down (Debugger : access Gdb_Debugger);

   procedure Stack_Up (Debugger : access Gdb_Debugger);

   procedure Break_Subprogram
     (Debugger : access Gdb_Debugger;
      Name     : String);

   procedure Break_Source
     (Debugger : access Gdb_Debugger;
      File     : String;
      Line     : Positive);

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Unhandled : Boolean := False);

   procedure Finish (Debugger : access Gdb_Debugger);

   function Backtrace (Debugger : access Gdb_Debugger) return String;

   function Info_Threads
     (Debugger : access Gdb_Debugger)
      return Language.Thread_Information_Array;

   function Line_Contains_Code
     (Debugger : access Gdb_Debugger;
      File     : String;
      Line     : Positive) return Boolean;

private

   type Gdb_Debugger is new Debugger.Debugger_Root with record
      Executable     : GNAT.OS_Lib.String_Access;
      Remote_Target  : Boolean;
      Target_Command : GNAT.OS_Lib.String_Access;
   end record;

end Debugger.Gdb;
