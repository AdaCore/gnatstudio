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

with System;            use System;
with GNAT.Regpat;       use GNAT.Regpat;
with GNAT.Expect;       use GNAT.Expect;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.Gdb.Ada;  use Debugger.Gdb.Ada;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Process_Proxies;   use Process_Proxies;

package body Debugger.Gdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher := Compile ("\(gdb\) ");
   --  Regular expressions used to recognize the prompt.

   Prompt_Length : constant := 6;
   --  Length of the prompt ("(gdb) ").

   Gdb_Command   : constant String := "gdb";
   --  Name of the command to launch gdb.

   Gdb_Options   : constant String := "-nw -q -fullname";
   --  Options always passed to gdb.

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Debugger : Gdb_Debugger; Entity : String) return String is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Wait (Get_Process (Debugger), Result, ".*", Timeout => 0);

      Send (Get_Process (Debugger), "ptype " & Entity);
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
      begin
         if S'Length > 14
           and then S (S'First .. S'First + 12) /= "No definition"
         then
            return S (S'First + 7 .. S'Last - Prompt_Length);
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
      Wait (Get_Process (Debugger), Result, ".*", Timeout => 0);
      Send (Get_Process (Debugger), "print " & Entity);
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
         Index : Natural := S'First;
      begin

         --  Skip the '$nn =' part
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

   procedure Spawn (Debugger       : in out Gdb_Debugger;
                    Arguments      : Argument_List;
                    Proxy          : Process_Proxies.Process_Proxy_Access;
                    Remote_Machine : String := "")
   is
      Num_Options     : Natural := Count (Gdb_Options, " ") + 1;
      Local_Arguments : Argument_List (1 .. Arguments'Length + Num_Options);
      First           : Natural := 1;
      Last            : Natural;

   begin
      --  Cut each blank separated word into an argument.
      --  Note that we assume here that only one blank is put between each
      --  option (in the computation of Num_Options).

      for J in 1 .. Num_Options - 1 loop
         Last := Index (Gdb_Options (First .. Gdb_Options'Last), " ");
         Local_Arguments (J) := new String' (Gdb_Options (First .. Last - 1));
         First := Index_Non_Blank (Gdb_Options (Last .. Gdb_Options'Last));
      end loop;

      Local_Arguments (Num_Options) :=
        new String' (Gdb_Options (First .. Gdb_Options'Last));
      Local_Arguments (Num_Options + 1 .. Local_Arguments'Last) := Arguments;

      General_Spawn
        (Debugger, Local_Arguments, Gdb_Command, Proxy, Remote_Machine);
      --  Add_Output_Filter (Debugger.Process.all, Trace_Filter'Access);
      --  Add_Input_Filter (Debugger.Process.all, Trace_Filter'Access);
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Gdb_Debugger) is
      Result     : Expect_Match;
      Matched    : GNAT.Regpat.Match_Array (0 .. 2);
      Descriptor : Process_Descriptor_Access := Get_Descriptor
        (Get_Process (Debugger.all));
   begin
      --  Wait for initial prompt
      Wait_Prompt (Debugger.all);

      Send (Get_Process (Debugger.all), "set prompt (gdb) ");
      Wait_Prompt (Debugger.all);
      Send (Get_Process (Debugger.all), "set width 0");
      Wait_Prompt (Debugger.all);
      Send (Get_Process (Debugger.all), "set height 0");
      Wait_Prompt (Debugger.all);
      Send (Get_Process (Debugger.all), "set annotate 1");
      Wait_Prompt (Debugger.all);
      Send (Get_Process (Debugger.all), "show lang");
      Expect
        (Descriptor.all, Result,
         "The current source language is ""(auto; currently )?([^""]+)""",
         Matched);

      declare
         S        : constant String := Expect_Out (Get_Process (Debugger.all));
         Lang     : String := S (Matched (2).First .. Matched (2).Last);
         Language : Language_Access;
      begin
         Wait_Prompt (Debugger.all);
         if Lang = "ada" then
            Language := new Gdb_Ada_Language;
         elsif Lang = "c" then
            Language := new Gdb_Ada_Language;
         else
            pragma Assert (False, "Language not currently supported");
            raise Program_Error;
         end if;

         Set_Language (Debugger.all, Language);
         Set_Debugger
           (Language_Debugger (Language.all), Debugger.all'Access);
      end;
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : in out Gdb_Debugger) is
      Result : Expect_Match;
   begin
      Send (Get_Process (Debugger), "quit");

      --  Ensure that gdb is terminated before closing the pipes and trying to
      --  kill it abruptly.

      Wait (Get_Process (Debugger), Result, ".*", Timeout => 2);
      Close (Get_Descriptor (Get_Process (Debugger)).all);
      Free (Debugger.Process);
   end Close;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Debugger : Gdb_Debugger; Executable : String) is
   begin
      Send (Get_Process (Debugger), "file " & Executable);
      Wait_Prompt (Debugger);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : Gdb_Debugger) is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "run");
      Wait_Prompt (Debugger);
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start (Debugger : Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "begin");
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into (Debugger : Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "step");
      Wait_Prompt (Debugger);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over (Debugger : Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "next");
      Wait_Prompt (Debugger);
   end Step_Over;

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
         Send (Get_Process (Debugger), "break exception unhandled");
      elsif Name /= "" then
         Send (Get_Process (Debugger), "break exception " & Name);
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
      Wait (Get_Process (Debugger), Result, ".*", Timeout => 0);
      Send (Get_Process (Debugger), "bt");
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
     (Debugger : Gdb_Debugger; Name : String) is
   begin
      Send (Get_Process (Debugger), "break " & Name);
      Wait_Prompt (Debugger);
   end Break_Subprogram;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "finish");
      Wait_Prompt (Debugger);
   end Finish;

   ------------------------
   -- Line_Contains_Code --
   ------------------------

   function Line_Contains_Code
     (Debugger : Gdb_Debugger;
      File     : String;
      Line     : Positive) return Boolean
   is
      Result : Expect_Match;
   begin
      --  Empty the buffer.
      Wait (Get_Process (Debugger), Result, ".*", Timeout => 0);

      Send (Get_Process (Debugger), "info line " & File & ':' &
        Positive'Image (Line));
      Wait_Prompt (Debugger);

      return Index
        (Expect_Out (Get_Process (Debugger)), "but contains no code") = 0;
   end Line_Contains_Code;

end Debugger.Gdb;
