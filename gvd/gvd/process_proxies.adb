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
with GNAT.IO;               use GNAT.IO;
with Gtk.Main;              use Gtk.Main;
with System;                use System;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body Process_Proxies is

   procedure Free (Post_Processes : in out Post_Process_Access);
   --  Free the list of post_processes pointed to by
   --  Post_Processes.

   procedure Process_Post_Processes (Proxy : access Process_Proxy'Class);
   --  Call all of the post-processes to be executed for Proxy.
   --  Free the list when the execution is completed.

   ----------
   -- Free --
   ----------

   procedure Free (Proxy : in out Process_Proxy_Access) is
      procedure Free_Internal is new Unchecked_Deallocation
        (Process_Proxy'Class, Process_Proxy_Access);
      procedure Free_Internal is new Unchecked_Deallocation
        (GNAT.Expect.Process_Descriptor,
         GNAT.Expect.Process_Descriptor_Access);
      procedure Free_Internal is new Unchecked_Deallocation
        (Boolean, Boolean_Access);
   begin
      Free_Internal (Proxy.Descriptor);
      Free_Internal (Proxy.Command_In_Process);
      Free (Proxy.Post_Processes);
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

   procedure Wait
     (Proxy   : access Process_Proxy;
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
      Process_Post_Processes (Proxy);
   end Wait;

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 20) is
   begin
      Proxy.Command_In_Process.all := True;

      if Timeout = -1 then
         Expect (Proxy.Descriptor.all, Result, Pattern, Matched, -1);
      else
         Expect (Proxy.Descriptor.all, Result, Pattern, Matched, Timeout * 50);
      end if;

      Proxy.Command_In_Process.all := False;
      Process_Post_Processes (Proxy);
   end Wait;

   procedure Wait
     (Proxy   : access Process_Proxy;
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
                   Matched : out GNAT.Regpat.Match_Array;
                   Timeout : Integer := 20)
   is
      Tmp   : Boolean;
      Num   : Integer := 1;
      Num_Events : Positive;
   begin

      --  ??? We should always avoid concurrent calls to Wait, or the exact
      --  behavior of the application will depend on specific timing, which is
      --  not reliable.
      if Proxy.Command_In_Process.all then
         Put_Line ("!!! already running a Wait command!!");
      end if;

      Proxy.Command_In_Process.all := True;

      --  We do not use a for loop, so that even if the timeout is 0 we
      --  execute the Expect call at least once.

      loop

         --  In case the external process was killed during the wait.

         if Proxy.Descriptor = null then
            exit;
         end if;

         Expect
           (Proxy.Descriptor.all, Result, Pattern, Matched,  Timeout => 10);

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

            when others =>
               --  It matched, we can simply return.
               exit;
         end case;
      end loop;

      Proxy.Command_In_Process.all := False;
      Process_Post_Processes (Proxy);
   end Wait;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : access Gui_Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : GNAT.Regpat.Pattern_Matcher;
                   Timeout : Integer := 20)
   is
      Matched : Match_Array (0 .. 0);
   begin
      Wait (Proxy, Result, Pattern, Matched, Timeout);
   end Wait;

   -------------------------
   -- Is_Internal_Command --
   -------------------------

   function Is_Internal_Command
     (Proxy : access Process_Proxy) return Boolean is
   begin
      return Proxy.Internal_Command_Stack (Proxy.Internal_Command);
   end Is_Internal_Command;

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

   ----------------
   -- TTY_Filter --
   ----------------

   procedure TTY_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Proxy      : System.Address)
   is
      function To_Proxy is new Unchecked_Conversion
        (System.Address, Process_Proxy_Access);
   begin
      if not Is_Internal_Command (To_Proxy (Proxy)) then
         Put (Str);
      end if;
   end TTY_Filter;

   -----------------------
   -- Register_Post_Cmd --
   -----------------------

   procedure Register_Post_Cmd
     (Proxy     : access Process_Proxy;
      Cmd       : Post_Process_Cmd;
      User_Data : System.Address)
   is
      Tmp : Post_Process_Access := Proxy.Post_Processes;
   begin
      if Tmp = null then
         Proxy.Post_Processes := new Post_Process_Record'
           (Cmd  => Cmd,
            Data => User_Data,
            Next => null);
      else
         while Tmp.Next /= null loop
            Tmp := Tmp.Next;
         end loop;
         Tmp.Next := new Post_Process_Record'
           (Cmd  => Cmd,
            Data => User_Data,
            Next => null);
      end if;
   end Register_Post_Cmd;

   ----------
   -- Free --
   ----------

   procedure Free (Post_Processes : in out Post_Process_Access)
   is
      procedure Free_Internal is new Unchecked_Deallocation
        (Post_Process_Record, Post_Process_Access);
      Tmp : Post_Process_Access;
   begin
      while Post_Processes /= null loop
         Tmp := Post_Processes.Next;
         Free_Internal (Post_Processes);
         Post_Processes := Tmp;
      end loop;
   end Free;

   ----------------------------
   -- Process_Post_Processes --
   ----------------------------

   procedure Process_Post_Processes (Proxy : access Process_Proxy'Class) is
      Initial : Post_Process_Access := Proxy.Post_Processes;
      Tmp : Post_Process_Access := Proxy.Post_Processes;
   begin
      --  We must reinitialize the list list of post_processes for the proxy
      --  before calling each of the command, otherwise if the command calls
      --  Wait as well, then the same list of post_cmds will be processed
      --  over and over again.

      Proxy.Post_Processes := null;

      while Tmp /= null loop
         Tmp.Cmd (Tmp.Data);
         Tmp := Tmp.Next;
      end loop;
      Free (Initial);
   end Process_Post_Processes;

end Process_Proxies;
