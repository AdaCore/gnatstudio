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

with Ada.Unchecked_Deallocation;
with GNAT.Expect;           use GNAT.Expect;
with GNAT.Regpat;           use GNAT.Regpat;

with Gtk.Main;
with Glib.Convert;          use Glib.Convert;

with GVD.Types;             use GVD.Types;

package body Process_Proxies is

   Timeout_Ms : constant := 50;
   --  Timeout in milliseconds for the low level calls to expect

   ----------
   -- Free --
   ----------

   procedure Free (Proxy : in out Process_Proxy_Access) is
      procedure Free_Internal is new Ada.Unchecked_Deallocation
        (Process_Proxy'Class, Process_Proxy_Access);
      procedure Free_Internal is new Ada.Unchecked_Deallocation
        (GNAT.Expect.Process_Descriptor'Class,
         GNAT.Expect.Process_Descriptor_Access);
      procedure Free_Internal is new Ada.Unchecked_Deallocation
        (Boolean, Boolean_Access);

   begin
      if Proxy /= null then
         Free_Internal (Proxy.Descriptor);
         Free_Internal (Proxy.Command_In_Process);
         Free_Internal (Proxy);
      end if;
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

   ----------------------------
   -- Set_Command_In_Process --
   ----------------------------

   procedure Set_Command_In_Process
     (Proxy      : access Process_Proxy;
      In_Process : Boolean := True) is
   begin
      Proxy.Command_In_Process.all := In_Process;
   end Set_Command_In_Process;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Parse_File_Switch) is
   begin
      if Self.Proxy = null then
         return;
      end if;

      Self.Work := Self.Proxy.Get_Parse_File_Name;
      if Self.Work then
         Self.Proxy.Set_Parse_File_Name (False);
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Parse_File_Switch) is
   begin
      if Self.Work then
         Self.Proxy.Set_Parse_File_Name (True);
         Self.Work := False;
      end if;
   end Finalize;

   -----------------
   -- Interrupted --
   -----------------

   function Interrupted (Proxy : access Process_Proxy) return Boolean is
   begin
      return Proxy.Interrupted;
   end Interrupted;

   ---------------------
   -- Set_Interrupted --
   ---------------------

   procedure Set_Interrupted
     (Proxy       : access Process_Proxy;
      Interrupted : Boolean := True) is
   begin
      Proxy.Interrupted := Interrupted;
   end Set_Interrupted;

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
         Wait (Proxy, Result, ".+", Timeout => 1);
      else
         Wait (Proxy, Result, ".*", Timeout => 1);
      end if;
   end Empty_Buffer;

   ----------
   -- Send --
   ----------

   procedure Send
     (Proxy : access Process_Proxy;
      Cmd : String;
      Empty_Buffer : Boolean := False) is
   begin
      Send (Proxy.Descriptor.all, Locale_From_UTF8 (Cmd), Add_LF => True,
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

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 1000) is
   begin
      Expect (Proxy.Descriptor.all, Result, Pattern, Timeout => Timeout);
   end Wait;

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 1000) is
   begin
      Expect (Proxy.Descriptor.all, Result, Pattern, Matched, Timeout);
   end Wait;

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : String;
      Timeout : Integer := 1000) is
   begin
      Wait (Proxy, Result, Compile (Pattern), Timeout);
   end Wait;

   overriding procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 1000)
   is
      Num        : Integer := Timeout_Ms;
      Num_Events : Positive;
      Max_Events : constant := 30;
      --  Limit the number of events to process in one iteration

      No_Main_Loop : Boolean;
      pragma Unreferenced (No_Main_Loop);

   begin
      --  Reset the interrupted flag before processing.

      Set_Interrupted (Proxy, False);

      --  We do not use a for loop, so that even if the timeout is 0 we
      --  execute the Expect call at least once.

      loop
         --  In case the external process was killed or interrupted during the
         --  wait.

         if Proxy.Descriptor = null then
            exit;
         end if;

         if Interrupted (Proxy) then
            --  Flush the output and exit

            Set_Interrupted (Proxy, False);
            Expect
              (Proxy.Descriptor.all,
               Result, Pattern, Matched, Timeout => Timeout);
            exit;
         end if;

         if Timeout = -1 then
            Expect
              (Proxy.Descriptor.all, Result, Pattern, Matched,
               Timeout => Timeout_Ms);
         else
            Expect
              (Proxy.Descriptor.all, Result, Pattern, Matched,
               Timeout => Integer'Min (Timeout_Ms, Timeout));
         end if;

         case Result is
            when Expect_Full_Buffer =>
               --  If the buffer was already full, we simply exit as if there
               --  had been a timeout. This should not be a problem in GVD,
               --  since the buffers have an unlimited size.
               exit;

            when Expect_Timeout =>
               --  Process any graphical event, and loop again.

               --  If we are already waiting, that means that one of the events
               --  that we processed in this loop led to a Wait command,
               --  and so that event must be simply discarded, since we want
               --  to process only graphical events here.

               Num_Events := 1;

               if not Proxy.Waiting then
                  Proxy.Waiting := True;

                  while Gtk.Main.Events_Pending
                    and then Num_Events <= Max_Events
                  loop
                     No_Main_Loop := Gtk.Main.Main_Iteration;
                     Num_Events := Num_Events + 1;
                  end loop;

                  Proxy.Waiting := False;

                  exit when Timeout = 0 or else Num >= Timeout;
                  Num := Num + Timeout_Ms;
               end if;

            when others =>
               --  It matched, we can simply return.
               exit;
         end case;
      end loop;
   end Wait;

   overriding procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 1000)
   is
      Matched : Match_Array (0 .. 0);
   begin
      Wait (Proxy, Result, Pattern, Matched, Timeout);
   end Wait;

   -------------------------
   -- Set_Parse_File_Name --
   -------------------------

   procedure Set_Parse_File_Name
     (Proxy : access Process_Proxy;
      Parse : Boolean) is
   begin
      Proxy.Parse_File_Name := Parse;
   end Set_Parse_File_Name;

   -------------------------
   -- Get_Parse_File_Name --
   -------------------------

   function Get_Parse_File_Name
     (Proxy : access Process_Proxy) return Boolean is
   begin
      return Proxy.Parse_File_Name;
   end Get_Parse_File_Name;

   ----------------------
   -- Get_Command_Mode --
   ----------------------

   function Get_Command_Mode
     (Proxy : access Process_Proxy) return Command_Type is
   begin
      return Proxy.Internal_Mode;
   end Get_Command_Mode;

   ----------------------
   -- Set_Command_Mode --
   ----------------------

   procedure Set_Command_Mode
     (Proxy : access Process_Proxy;
      Mode  : Command_Type) is
   begin
      Proxy.Internal_Mode := Mode;
   end Set_Command_Mode;

end Process_Proxies;
