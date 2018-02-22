------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with GNAT.Expect;               use GNAT.Expect;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Debugger;                  use Debugger;
with GVD.Preferences;           use GVD.Preferences;
with Process_Proxies;           use Process_Proxies;
with System;

package body GVD.Trace is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.GVD_OUT");
   Me_Large : constant Trace_Handle := Create
     ("GPS.DEBUGGING.GVD_OUT_LARGE", Off);

   Max_Lines : constant := 3;
   --  Maximum number of lines to output for each command, in GVD.OUT.
   --  Extra output is sent to GVD.OUT.LARGE

   type IO_Kind is (Input_Kind, Output_Kind);
   --  Kind of IO in the log files.
   --  Input_Kind means input strings sent to the debugger.
   --  Output_Kind means output strings received from the debugger.

   Direction_String : constant array (IO_Kind) of String (1 .. 4) :=
     (Input_Kind  => "-> """,
      Output_Kind => "<- """);

   pragma Warnings (Off);
   --  This UC is safe aliasing-wise, so kill warning
   function To_Process is new
     Ada.Unchecked_Conversion (System.Address, Visual_Debugger);
   pragma Warnings (On);

   procedure Input_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      Process    : System.Address := System.Null_Address);
   --  Filter that should be called when GVD receives inputs.

   procedure Output_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      Process    : System.Address := System.Null_Address);
   --  Filter that should be called when GVD receives output from the
   --  underlying debugger.

   procedure Output_Message
     (Process : Visual_Debugger;
      Str     : String;
      Kind    : IO_Kind := Input_Kind);
   --  Write on the log file associated with Process.
   --  Replace ASCII.LF by "\n" and ASCII.HT by "\t", and put lines in quotes.

   ------------------
   -- Output_Error --
   ------------------

   procedure Output_Error
     (Kernel : access Kernel_Handle_Record'Class;
      Str    : String) is
   begin
      GNATCOLL.Traces.Trace (Me, "# " & Str);
      Kernel.Insert (Str, Mode => GPS.Kernel.Error);
   end Output_Error;

   --------------------
   -- Output_Message --
   --------------------

   procedure Output_Message
     (Process : Visual_Debugger;
      Str     : String;
      Kind    : IO_Kind := Input_Kind)
   is
      Num        : constant String := Integer'Image (Process.Debugger_Num);
      Output     : String (1 .. Str'Length * 2);
      pragma Warnings (Off, Output);
      Index      : Natural := Output'First;
      Prefix     : constant String := '[' & Num (2 .. Num'Last) & "] ";
      Had_Output : Boolean := False;
      H          : Trace_Handle;

   begin
      if Kind = Input_Kind
         or else Process.Log_Lines <= Max_Lines
      then
         H := Me;
      else
         if Process.Log_Lines = Max_Lines + 1 then
            GNATCOLL.Traces.Trace (Me, "[...]");
         end if;

         H := Me_Large;
      end if;

      Process.Log_Lines := Process.Log_Lines + 1;

      for J in Str'Range loop
         case Str (J) is
            when ASCII.LF =>
               if not Had_Output then
                  GNATCOLL.Traces.Trace (H, Prefix
                         & Direction_String (Kind)
                         & Output (Output'First .. Index - 1)
                         & '"');
                  Had_Output := True;
               else
                  GNATCOLL.Traces.Trace (H, Prefix & "..."
                         & Direction_String (Kind)
                         & Output (Output'First .. Index - 1)
                         & '"');
               end if;
               Index := Output'First;
            when ASCII.CR =>
               Output (Index)     := '\';
               Output (Index + 1) := 'r';
               Index := Index + 2;
            when ASCII.HT =>
               Output (Index)     := '\';
               Output (Index + 1) := 't';
               Index := Index + 2;
            when others =>
               Output (Index) := Str (J);
               Index := Index + 1;
         end case;
      end loop;

      if Index > Output'First then
         if not Had_Output then
            GNATCOLL.Traces.Trace (H, Prefix
                   & Direction_String (Kind)
                   & Output (Output'First .. Index - 1)
                   & '"');
         else
            GNATCOLL.Traces.Trace (H, Prefix & "..."
                   & Direction_String (Kind)
                   & Output (Output'First .. Index - 1)
                   & '"');
         end if;
      end if;
   end Output_Message;

   ------------------
   -- Input_Filter --
   ------------------

   procedure Input_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      Process    : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor);
      P : constant Visual_Debugger := To_Process (Process);
   begin
      if Debugger_Console_All_Interactions.Get_Pref then
         P.Output_Text (Str);
      end if;

      if P.Store_History then
         P.Interactions_History.Append (Str);
      end if;

      Output_Message (P, Str, Input_Kind);
      P.Log_Lines := 1;
   end Input_Filter;

   -------------------
   -- Output_Filter --
   -------------------

   procedure Output_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      Process    : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor);
      P : constant Visual_Debugger := To_Process (Process);
   begin
      if Debugger_Console_All_Interactions.Get_Pref then
         P.Output_Text (Str);
      end if;

      if P.Store_History then
         P.Interactions_History.Append (Str);
      end if;

      Output_Message (P, Str, Output_Kind);
   end Output_Filter;

   -----------------------------
   -- Set_Input_Output_Filter --
   -----------------------------

   procedure Set_Input_Output_Filter
     (Process : not null access Visual_Debugger_Record'Class)
   is
      D : constant Process_Descriptor_Access :=
        Get_Descriptor (Get_Process (Process.Debugger));
   begin
      Add_Filter
        (D.all,
         Output_Filter'Access, Output,
         Process.all'Address);
      Add_Filter
        (D.all,
         Input_Filter'Access, Input,
         Process.all'Address);
   end Set_Input_Output_Filter;

end GVD.Trace;
