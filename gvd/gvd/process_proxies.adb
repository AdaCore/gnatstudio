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

with GNAT.Expect;           use GNAT.Expect;
with GNAT.Regpat;           use GNAT.Regpat;
with Gtk.Main;              use Gtk.Main;
with System;                use System;
with Unchecked_Deallocation;

with Ada.Text_IO; use Ada.Text_IO;

package body Process_Proxies is

   ----------
   -- Free --
   ----------

   procedure Free (Proxy : in out Process_Proxy_Access) is
      procedure Free_Internal is new Unchecked_Deallocation
        (Process_Proxy'Class, Process_Proxy_Access);
      procedure Free_Internal is new Unchecked_Deallocation
        (GNAT.Expect.Process_Descriptor,
         GNAT.Expect.Process_Descriptor_Access);
   begin
      Free_Internal (Proxy.Descriptor);
      Free_Internal (Proxy);
   end Free;

   --------------------
   -- Get_Descriptor --
   --------------------

   function Get_Descriptor
     (Proxy : access Process_Proxy) return Process_Descriptor_Access is
   begin
      return Proxy.Descriptor;
   end Get_Descriptor;

   --------------------
   -- Set_Descriptor --
   --------------------

   procedure Set_Descriptor
     (Proxy      : access Process_Proxy;
      Descriptor : GNAT.Expect.Process_Descriptor_Access) is
   begin
      Proxy.Descriptor := Descriptor;
   end Set_Descriptor;

   ------------------------
   -- Command_In_Process --
   ------------------------

   function Command_In_Process (Proxy : access Process_Proxy) return Boolean is
   begin
      return Proxy.Command_In_Process.all;
   end Command_In_Process;

   ------------------
   -- Empty_Buffer --
   ------------------

   procedure Empty_Buffer
     (Proxy        : access Process_Proxy;
      At_Least_One : Boolean := False)
   is
      Result : GNAT.Expect.Expect_Match;
   begin
      if At_Least_One then
         Wait (Proxy, Result, ".+", Timeout => 0);
      else
         Wait (Proxy, Result, ".*", Timeout => 0);
      end if;
   end Empty_Buffer;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : access Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : GNAT.Regpat.Pattern_Matcher;
                   Timeout : Integer := 20) is
   begin
      Proxy.Command_In_Process.all := True;

      --  In text mode, there is no race condition with an output filter,
      --  so we go for the simple solution.

      if Timeout = -1 then
         Expect (Proxy.Descriptor.all, Result, Pattern, Timeout => -1);
      else
         Expect
           (Proxy.Descriptor.all, Result, Pattern, Timeout => Timeout * 50);
      end if;

      Proxy.Command_In_Process.all := False;
   end Wait;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : access Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : String;
                   Timeout : Integer := 20) is
   begin
      Wait (Proxy, Result, Compile (Pattern), Timeout);
   end Wait;

   ----------
   -- Send --
   ----------

   procedure Send
     (Proxy : access Process_Proxy;
      Cmd : String;
      Empty_Buffer : Boolean := False) is
   begin
      Send (Proxy.Descriptor.all, Cmd, Add_LF => True,
            Empty_Buffer => Empty_Buffer);
   end Send;

   ----------------
   -- Expect_Out --
   ----------------

   function Expect_Out (Proxy : access Process_Proxy) return String is
   begin
      return Expect_Out (Proxy.Descriptor.all);
   end Expect_Out;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : access Gui_Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : GNAT.Regpat.Pattern_Matcher;
                   Timeout : Integer := 20)
   is
      Tmp   : Boolean;
      Num   : Integer := 1;
      Num_Events : Positive;
   begin
      Proxy.Command_In_Process.all := True;

      --  We do not use a for loop, so that even if the timeout is 0 we
      --  execute the Expect call at least once.

      loop

         --  In case the external process was killed during the wait.

         if Proxy.Descriptor = null then
            exit;
         end if;

         Expect (Proxy.Descriptor.all, Result, Pattern, Timeout => 50);

         exit when Num = Timeout;

         Num := Num + 1;

         case Result is

            when Expect_Full_Buffer =>
               --  If the buffer was already full, we simply exit as if there
               --  had been a timeout. This should not be a problem in odd,
               --  since the buffers have an unlimited size.
               exit;

            when Expect_Timeout =>

               --  Process the X events, and loop again.
               --  For efficiency, we stop after a certain number. Otherwise,
               --  it sometimes happens that we keep getting events (input
               --  events ?), and we never exit this loop.
               --  ??? This might not be the best workaround.

               Num_Events := 1;
               while Gtk.Main.Events_Pending
                 and then Num_Events <= 30
               loop
                  Tmp := Gtk.Main.Main_Iteration;
                  Num_Events := Num_Events + 1;
               end loop;

               --  While (G_Main_Iteration (False)); is recommended in C ?

            when others =>
               --  It matched, we can simply return.
               exit;
         end case;
      end loop;

      Proxy.Command_In_Process.all := False;
   end Wait;

   -------------------------
   -- Is_Internal_Command --
   -------------------------

   function Is_Internal_Command (Proxy : access Process_Proxy)
                                return Boolean
   is
   begin
      return Proxy.Internal_Command_Stack (Proxy.Internal_Command);
   end Is_Internal_Command;

   -------------------------
   -- Set_Parse_File_Name --
   -------------------------

   procedure Set_Parse_File_Name (Proxy : access Process_Proxy;
                                  Parse : Boolean)
   is
   begin
      Proxy.Parse_File_Name := Parse;
   end Set_Parse_File_Name;

   -------------------------
   -- Get_Parse_File_Name --
   -------------------------

   function Get_Parse_File_Name (Proxy : access Process_Proxy)
                                return Boolean
   is
   begin
      return Proxy.Parse_File_Name;
   end Get_Parse_File_Name;

   ----------------------------------
   -- Push_Internal_Command_Status --
   ----------------------------------

   procedure Push_Internal_Command_Status
     (Proxy       : access Process_Proxy;
      Is_Internal : Boolean)
   is
   begin
      if Proxy.Internal_Command = Internal_Status_Stack_Size then
         raise Internal_Command_Status_Stack_Overfull;
      end if;
      Proxy.Internal_Command := Proxy.Internal_Command + 1;
      Proxy.Internal_Command_Stack (Proxy.Internal_Command) := Is_Internal;
   end Push_Internal_Command_Status;

   ---------------------------------
   -- Pop_Internal_Command_Status --
   ---------------------------------

   procedure Pop_Internal_Command_Status (Proxy : access Process_Proxy) is
   begin
      --  Constraint_Error will be raised if we are trying to pop too many
      --  values.
      Proxy.Internal_Command := Proxy.Internal_Command - 1;
   end Pop_Internal_Command_Status;

end Process_Proxies;
