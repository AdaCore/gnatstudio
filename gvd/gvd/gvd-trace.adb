-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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
with GVD.Types; use GVD.Types;
with Process_Proxies; use Process_Proxies;
with Debugger; use Debugger;
with System;
with Unchecked_Conversion;

package body GVD.Trace is

   Input_String  : aliased constant String := "-> """;
   Output_String : aliased constant String := "<- """;
   Quote_EOL     : aliased constant String := '"' & ASCII.LF;
   Quote_SOL     : aliased constant String := "       """;
   Verbose_EOL   : aliased constant String := "\n";
   Verbose_HT    : aliased constant String := "\t";

   function To_Main_Window is new
     Unchecked_Conversion (System.Address, Main_Debug_Window_Access);

   --------------------
   -- Output_Message --
   --------------------

   procedure Output_Message
     (Process : Debugger_Process_Tab;
      Str     : String;
      Mode    : Command_Type;
      Kind    : IO_Kind := Input_Kind)
   is
      N      : Integer;
      File   : File_Descriptor renames Process.Window.Log_File;
      Num    : constant String := Integer'Image (Process.Debugger_Num);
      Prefix : aliased constant String := '[' & Num (2 .. Num'Last) & "] ";

   begin
      if Mode < Process.Window.Log_Level then
         return;
      end if;

      if Kind = Input_Kind then
         N := Write (File, Prefix'Address, Prefix'Length);
         N := Write (File, Input_String'Address, Input_String'Length);

      elsif Kind = Output_Kind then
         N := Write (File, Prefix'Address, Prefix'Length);
         N := Write (File, Output_String'Address, Output_String'Length);

      else
         raise Program_Error;
      end if;

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

      N := Write (File, Quote_EOL'Address, Quote_EOL'Length);
   end Output_Message;

   ------------------
   -- Input_Filter --
   ------------------

   procedure Input_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      Window : constant Main_Debug_Window_Access := To_Main_Window (User_Data);
   begin
      declare
         Tab : constant Debugger_Process_Tab := Convert (Window, Descriptor);
      begin
         Output_Message
           (Tab, Str,
            Get_Command_Mode (Get_Process (Tab.Debugger)), Input_Kind);
      exception
         when Debugger_Not_Found => null;
      end;
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
   begin
      declare
         Tab : constant Debugger_Process_Tab := Convert (Window, Descriptor);
      begin
         Output_Message
           (Tab, Str,
            Get_Command_Mode (Get_Process (Tab.Debugger)), Output_Kind);
      exception
         when Debugger_Not_Found => null;
      end;
   end Output_Filter;

end GVD.Trace;
