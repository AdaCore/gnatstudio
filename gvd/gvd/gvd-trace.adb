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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Odd.Process; use Odd.Process;
with Odd.Types; use Odd.Types;
with Process_Proxies; use Process_Proxies;
with Debugger; use Debugger;
with System;
with Unchecked_Conversion;

package body Odd.Trace is

   Input_String  : aliased constant String := "-> """;
   Output_String : aliased constant String := "<- """;
   Quote_EOL     : aliased constant String := '"' & ASCII.LF;
   Quote_SOL     : aliased constant String := "       """;
   Verbose_EOL   : aliased constant String := "\n";
   Verbose_HT    : aliased constant String := "\t";

   function To_Main_Window is new
     Unchecked_Conversion (System.Address, Main_Debug_Window_Access);

   procedure Output_Message (File : File_Descriptor; Str : String);
   --  Write on File the string Str, by replacing ASCII.LF by "\n" and
   --  ASCII.HT by "\t", and putting lines in quotes.

   procedure Output_Message (File : File_Descriptor; Str : String) is
      N : Integer;
   begin
      for J in Str'Range loop
         case Str (J) is
            when ASCII.LF =>
               N := Write (File, Verbose_EOL'Address, Verbose_EOL'Length);

               if J < Str'Last then
                  N := Write (File, Quote_EOL'Address, Quote_EOL'Length);
                  N := Write (File, Quote_SOL'Address, Quote_SOL'Length);
               end if;

            when ASCII.HT =>
               N := Write (File, Verbose_HT'Address, Verbose_HT'Length);
            when others =>
               N := Write (File, Str (J)'Address, 1);
         end case;
      end loop;
   end Output_Message;

   ------------------
   -- Input_Filter --
   ------------------

   procedure Input_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      Window       : constant Main_Debug_Window_Access :=
        To_Main_Window (User_Data);
      N            : Integer;

   begin
      declare
         Tab    : constant Debugger_Process_Tab :=
           Convert (Window, Descriptor);
         Num    : constant String := Integer'Image (Tab.Debugger_Num);
         Prefix : aliased constant String := '[' & Num (2 .. Num'Last) & "] ";

      begin
         if Get_Command_Mode (Get_Process (Tab.Debugger)) >=
           Window.Log_Level
         then
            N := Write (Window.Log_File, Prefix'Address, Prefix'Length);
            N := Write
              (Window.Log_File, Input_String'Address, Input_String'Length);
            Output_Message (Window.Log_File, Str);
            N := Write (Window.Log_File, Quote_EOL'Address, Quote_EOL'Length);
         end if;
      end;

   exception
      when Debugger_Not_Found => null;
   end Input_Filter;

   -------------------
   -- Output_Filter --
   -------------------

   procedure Output_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      Window : constant Main_Debug_Window_Access := To_Main_Window (User_Data);
      N      : Integer;

   begin
      declare
         Tab    : constant Debugger_Process_Tab :=
           Convert (Window, Descriptor);
         Num    : constant String := Integer'Image (Tab.Debugger_Num);
         Prefix : aliased constant String := '[' & Num (2 .. Num'Last) & "] ";

      begin
         if Get_Command_Mode (Get_Process (Tab.Debugger)) >=
           Window.Log_Level
         then
            N := Write (Window.Log_File, Prefix'Address, Prefix'Length);
            N := Write
              (Window.Log_File, Output_String'Address, Output_String'Length);
            Output_Message (Window.Log_File, Str);
            N := Write (Window.Log_File, Quote_EOL'Address, Quote_EOL'Length);
         end if;
      end;

   exception
      when Debugger_Not_Found => null;
   end Output_Filter;

end Odd.Trace;
