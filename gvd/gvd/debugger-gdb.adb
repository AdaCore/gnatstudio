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

with Generic_Values; use Generic_Values;
with System;         use System;
with GNAT.Regpat;    use GNAT.Regpat;
with GNAT.Expect;    use GNAT.Expect;
with GNAT.OS_Lib;    use GNAT.OS_Lib;

package body Debugger.Gdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher := Compile ("\(gdb\) ");
   --  Regular expressions used to recognize the prompt.

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     (Debugger : Gdb_Debugger;
      Entity   : String) return Generic_Values.Generic_Type_Access
   is
      Result : Generic_Type_Access;
      Type_Str : String := Type_Of (Debugger, Entity);
      Index  : Natural := Type_Str'First;

   begin
      if Type_Str'Length /= 0 then
         Language.Parse_Type
           (Debugger.The_Language.all, Type_Str, Entity, Index, Result);
      end if;

      return Result;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Debugger  : Gdb_Debugger;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access)
   is
      Type_Str   : String := Value_Of (Debugger, Entity);
      Index      : Natural := Type_Str'First;
      Repeat_Num : Positive;

   begin
      --  Clear the value previously parsed.
      Clear_Value (Value.all);
      Language.Parse_Value
        (Debugger.The_Language.all, Type_Str, Index, Value, Repeat_Num);
   end Parse_Value;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Debugger : Gdb_Debugger; Entity : String) return String is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);

      Send (Debugger.Process.all, "ptype " & Entity);
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Debugger.Process.all);
      begin
         if S'Length > 14 then
            return S (S'First + 7 .. S'Last - 6);
         else
            return "";
         end if;
      end;
   end Type_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Debugger : Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String
   is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);

      Send (Debugger.Process.all, "print " & Entity);
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Debugger.Process.all);
         Index : Natural := S'First;
      begin

         --  Skip the '$nn =' part
         while Index <= S'Last
           and then S (Index) /= '='
         loop
            Index := Index + 1;
         end loop;
         Index := Index + 1;

         return S (Index + 1 .. S'Last - 6);
      end;
   end Value_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : in out Gdb_Debugger) is
      Null_List : GNAT.OS_Lib.Argument_List (1 .. 0);
   begin
      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      Debugger.Process :=
        new Pipes_Id'(Non_Blocking_Spawn ("gdb", Null_List, Buffer_Size => 0));

--        Add_Output_Filter (Debugger.Process.all, Trace_Filter'Access);
--        Add_Input_Filter (Debugger.Process.all, Trace_Filter'Access);
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set prompt (gdb) ");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set width 0");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set height 0");
      Wait_Prompt (Debugger);
      Send (Debugger.Process.all, "set annotate 1");
      Wait_Prompt (Debugger);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : in out Gdb_Debugger) is
   begin
      Close (Debugger.Process.all);
      Free (Debugger.Process);
   end Close;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Debugger : Gdb_Debugger; Executable : String) is
   begin
      Send (Debugger.Process.all, "file " & Executable);
      Wait_Prompt (Debugger);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : Gdb_Debugger) is
      Num : Expect_Match;
   begin
      Expect (Debugger.Process.all, Num, Prompt_Regexp);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : Gdb_Debugger) is
   begin
      Send (Debugger.Process.all, "run");
   end Run;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception (Debugger  : Gdb_Debugger;
                              Name      : String  := "";
                              Unhandled : Boolean := False)
   is
   begin
      --  ??? If language = "Ada"
      if Unhandled then
         Send (Debugger.Process.all, "break exception unhandled");
      elsif Name /= "" then
         Send (Debugger.Process.all, "break exception " & Name);
      else
         raise Unknown_Command;
      end if;
      Wait_Prompt (Debugger);
   end Break_Exception;

   ---------------
   -- Backtrace --
   ---------------

   function Backtrace (Debugger : Gdb_Debugger) return String is
      Result : Expect_Match;
   begin
      Expect (Debugger.Process.all, Result, ".*", Timeout => 0);
      Send (Debugger.Process.all, "bt");
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Debugger.Process.all);
      begin
         return S (S'First .. S'Last - 6);
      end;
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger : Gdb_Debugger; Name : String) is
   begin
      Send (Debugger.Process.all, "break " & Name);
      Wait_Prompt (Debugger);
   end Break_Subprogram;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : Gdb_Debugger) is
   begin
      Send (Debugger.Process.all, "finish");
      Wait_Prompt (Debugger);
   end Finish;

end Debugger.Gdb;
