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
with Gtk.Window;
with Language.Debugger;
with Odd.Types;

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

   function Source_Files_List (Debugger : access Gdb_Debugger)
                              return Odd.Types.String_Array;

   function Type_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String;

   function Info_Locals (Debugger : access Gdb_Debugger) return String;

   function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : String);

   procedure Run
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Start
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Step_Into
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Step_Over
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Continue
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Interrupt (Debugger : access Gdb_Debugger);

   function Is_Execution_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean;

   function Is_Context_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean;

   function Is_Break_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean;

   procedure Stack_Down
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Stack_Up
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Stack_Frame
     (Debugger : access Gdb_Debugger;
      Frame    : Positive;
      Display  : Boolean := False);

   procedure Break_Subprogram
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Display   : Boolean := False);

   procedure Break_Source
     (Debugger  : access Gdb_Debugger;
      File      : String;
      Line      : Positive;
      Temporary : Boolean := False;
      Display   : Boolean := False);

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Display   : Boolean := False);

   procedure Break_Address
     (Debugger   : access Gdb_Debugger;
      Address    : String;
      Temporary  : Boolean := False;
      Display    : Boolean := False);

   procedure Break_Regexp
     (Debugger   : access Gdb_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Display    : Boolean := False);

   procedure Enable_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : Integer;
      Enable   : Boolean := True;
      Display  : Boolean := False);

   procedure Remove_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : Integer;
      Display  : Boolean := False);

   procedure Finish
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False);

   procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural);

   function Variable_Name_With_Frame
     (Debugger : access Gdb_Debugger;
      Var      : String)
     return String;

   function Info_Threads
     (Debugger : access Gdb_Debugger)
      return Language.Thread_Information_Array;

   function Line_Contains_Code
     (Debugger : access Gdb_Debugger;
      File     : String;
      Line     : Positive) return Line_Kind;

   function List_Breakpoints
     (Debugger  : access Gdb_Debugger)
     return Odd.Types.Breakpoint_Array;

   function List_Exceptions
     (Debugger : access Gdb_Debugger)
     return Odd.Types.Exception_Array;

private

   function Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Display         : Boolean := False;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True)
     return String;

   type Gdb_Debugger is new Debugger.Debugger_Root with record
      Executable     : GNAT.OS_Lib.String_Access;
      Remote_Target  : Boolean;
      Target_Command : GNAT.OS_Lib.String_Access;
   end record;

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive;
      Parent     : Items.Generic_Type_Access);
   --  Internal function used to parse the value.
   --  The parameters are the same as for Parse_Value, plus Parent that is
   --  the item that contains Result.
   --  Parent should be null for the top-level item.

end Debugger.Gdb;
