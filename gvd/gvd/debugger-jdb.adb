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
with Debugger.Jdb.Java; use Debugger.Jdb.Java;
with Process_Proxies;   use Process_Proxies;

package body Debugger.Jdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher :=
     Compile ("^> ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.

   Prompt_Length : constant := 2;
   --  Length of the prompt ("> ").

   Highlight_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("^> ");
   --  Match everything that should be highlighted in the debugger window.

   Jdb_Command : constant String := "jdb";
   --  The default Jdb executable name.

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Debugger : access Jdb_Debugger; Entity : String) return String is
   begin
      return "";
   end Type_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Debugger : access Jdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String is
   begin
      --  Empty the buffer.
      Empty_Buffer (Get_Process (Debugger));
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

   procedure Spawn
     (Debugger        : access Jdb_Debugger;
      Executable      : String;
      Arguments       : GNAT.OS_Lib.Argument_List;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") is
   begin
      if Debugger_Name = "" then
         General_Spawn (Debugger, Arguments, Jdb_Command, Proxy, Remote_Host);
      else
         General_Spawn
           (Debugger, Arguments, Debugger_Name, Proxy, Remote_Host);
      end if;

      Free (Debugger.Main_Class);

      if Executable /= "" then
         Debugger.Main_Class := new String' (Executable);
      end if;

      Add_Output_Filter
        (Get_Descriptor (Debugger.Process).all,
         Trace_Filter'Access);
      Add_Input_Filter
        (Get_Descriptor (Debugger.Process).all,
         Trace_Filter'Access);
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Jdb_Debugger) is
      Language   : Language_Access;
   begin
      --  Wait for initial prompt
      Set_Internal_Command (Get_Process (Debugger), False);
      Wait_Prompt (Debugger);
      Set_Internal_Command (Get_Process (Debugger), True);

      if Debugger.Main_Class /= null then
         Set_Executable (Debugger, Debugger.Main_Class.all);
      end if;

      Language := new Jdb_Java_Language;
      Set_Language (Debugger, Language);
      Set_Debugger (Language_Debugger_Access (Language), Debugger.all'Access);
      Set_Internal_Command (Get_Process (Debugger), False);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Jdb_Debugger) is
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

   procedure Set_Executable
     (Debugger : access Jdb_Debugger;
      Executable : String) is
   begin
      Send (Get_Process (Debugger), "load " & Executable);
      Wait_Prompt (Debugger);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : access Jdb_Debugger) is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "run");
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start (Debugger : access Jdb_Debugger) is
   begin
      --  Send (Get_Process (Debugger).all, "run");
      null;
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "step");
      Wait_Prompt (Debugger);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "next");
      Wait_Prompt (Debugger);
   end Step_Over;

   --------------
   -- Continue --
   --------------

   procedure Continue (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "cont");
      Wait_Prompt (Debugger);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Debugger : access Jdb_Debugger) is
   begin
      Interrupt (Get_Descriptor (Get_Process (Debugger)).all);
   end Interrupt;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "down");
      Wait_Prompt (Debugger);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "up");
      Wait_Prompt (Debugger);
   end Stack_Up;

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Debugger : access Jdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural) is
   begin
      Empty_Buffer (Get_Process (Debugger));
      Send (Get_Process (Debugger), "where");
      Wait_Prompt (Debugger);
      declare
         S : String := Expect_Out (Get_Process (Debugger));
      begin
         Len := 0;
      end;
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger : access Jdb_Debugger; Name : String) is
   begin
      Send (Get_Process (Debugger), "stop in " & Name);
      Wait_Prompt (Debugger);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Debugger : access Jdb_Debugger;
      File     : String;
      Line     : Positive)
   is
      Str : constant String := Positive'Image (Line);
      Pos : Positive;
   begin
      Pos := File'Last;

      --  Remove the extension from the filename to get an estimation of the
      --  class name.

      while Pos > File'First and then File (Pos) /= '.' loop
         Pos := Pos - 1;
      end loop;

      if File (Pos) = '.' then
         Pos := Pos - 1;
      else
         --  No file extension, assume a valid class name.
         Pos := File'Last;
      end if;

      Send (Get_Process (Debugger),
        "stop at " & File (File'First .. Pos) & ':' &
        Str (Str'First + 1 .. Str'Last));
      Wait_Prompt (Debugger);
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Debugger  : access Jdb_Debugger;
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

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "step up");
      Wait_Prompt (Debugger);
   end Finish;

   ------------------
   -- Info_Threads --
   ------------------

   function Info_Threads
     (Debugger : access Jdb_Debugger)
      return Language.Thread_Information_Array is
   begin
      Empty_Buffer (Get_Process (Debugger));
      Send (Get_Process (Debugger), Thread_List (Get_Language (Debugger)));
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
      begin
         return Parse_Thread_List
           (Get_Language (Debugger), S (S'First .. S'Last - Prompt_Length));
      end;
   end Info_Threads;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   function Highlighting_Pattern
     (Debugger : access Jdb_Debugger) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   ------------------------
   -- Line_Contains_Code --
   ------------------------

   function Line_Contains_Code
     (Debugger : access Jdb_Debugger;
      File     : String;
      Line     : Positive) return Boolean is
   begin
      return False;
   end Line_Contains_Code;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt (Debugger : access Jdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "  ");
      Wait_Prompt (Debugger);
   end Display_Prompt;

end Debugger.Jdb;
