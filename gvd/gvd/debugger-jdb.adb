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

with Generic_Values;    use Generic_Values;
with System;            use System;
with GNAT.Regpat;       use GNAT.Regpat;
with GNAT.Expect;       use GNAT.Expect;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.Jdb.Java; use Debugger.Jdb.Java;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Process_Proxies;   use Process_Proxies;

package body Debugger.Jdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher := Compile ("> ");
   --  Regular expressions used to recognize the prompt.

   Prompt_Length : constant := 2;
   --  Length of the prompt ("> ").

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Debugger : Jdb_Debugger; Entity : String) return String is
   begin
      return "";
   end Type_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Debugger : Jdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String
   is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Wait (Get_Process (Debugger), Result, ".*", Timeout => 0);
      Send (Get_Process (Debugger), "print " & Entity);
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
         Index : Natural := S'First;
      begin

         --  Skip the 'var =' part
         while Index <= S'Last
           and then S (Index) /= '='
         loop
            Index := Index + 1;
         end loop;

         Index := Index + 1;

         return S (Index + 1 .. S'Last - Prompt_Length);
      end;
   end Value_Of;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Debugger       : in out Jdb_Debugger;
                    Arguments      : Argument_List;
                    Proxy          : Process_Proxies.Process_Proxy_Access;
                    Remote_Machine : String := "") is
   begin
      General_Spawn (Debugger, Arguments, "jdb", Proxy, Remote_Machine);
      --  Add_Output_Filter (Debugger.Process.all, Trace_Filter'Access);
      --  Add_Input_Filter (Debugger.Process.all, Trace_Filter'Access);
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Jdb_Debugger) is
      Result     : Expect_Match;
      Matched    : GNAT.Regpat.Match_Array (0 .. 2);
      Descriptor : Process_Descriptor_Access := Get_Descriptor
        (Get_Process (Debugger.all));
      Language   : Language_Access;
   begin
      --  Wait for initial prompt
      Wait_Prompt (Debugger.all);
      Language := new Jdb_Java_Language;
      Set_Language (Debugger.all, Language);
      Set_Debugger (Language_Debugger (Language.all), Debugger.all'Access);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : in out Jdb_Debugger) is
      Result : Expect_Match;
   begin
      Send (Get_Process (Debugger), "quit");

      --  Ensure that jdb is terminated before closing the pipes and trying to
      --  kill it abruptly.

      Wait (Get_Process (Debugger), Result, ".*", Timeout => 2);
      Close (Get_Descriptor (Get_Process (Debugger)).all);
      Free (Debugger.Process);
   end Close;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Debugger : Jdb_Debugger; Executable : String) is
   begin
      Send (Get_Process (Debugger), "load " & Executable);
      Wait_Prompt (Debugger);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : Jdb_Debugger) is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "run");
      Wait_Prompt (Debugger);
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start (Debugger : Jdb_Debugger) is
   begin
      --  Send (Get_Process (Debugger), "run");
      null;
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into (Debugger : Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "step");
      Wait_Prompt (Debugger);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over (Debugger : Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "next");
      Wait_Prompt (Debugger);
   end Step_Over;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception (Debugger  : Jdb_Debugger;
                              Name      : String  := "";
                              Unhandled : Boolean := False) is
   begin
      if Unhandled then
         raise Unknown_Command;
      else
         Send (Get_Process (Debugger), "catch " & Name);
      end if;

      Wait_Prompt (Debugger);
   end Break_Exception;

   ---------------
   -- Backtrace --
   ---------------

   function Backtrace (Debugger : Jdb_Debugger) return String is
      Result : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Result, ".*", Timeout => 0);
      Send (Get_Process (Debugger), "where");
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Get_Process (Debugger));
      begin
         return S (S'First .. S'Last - Prompt_Length);
      end;
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger : Jdb_Debugger; Name : String) is
   begin
      Send (Get_Process (Debugger), "stop in " & Name);
      Wait_Prompt (Debugger);
   end Break_Subprogram;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "step up");
      Wait_Prompt (Debugger);
   end Finish;

   ------------------------
   -- Line_Contains_Code --
   ------------------------

   function Line_Contains_Code
     (Debugger : Jdb_Debugger;
      File     : String;
      Line     : Positive) return Boolean is
   begin
      return False;
   end Line_Contains_Code;

end Debugger.Jdb;
