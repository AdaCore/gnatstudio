-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

pragma Warnings (Off);
with GNAT.Expect;           use GNAT.Expect;
pragma Warnings (On);

with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.IO;               use GNAT.IO;
with Gtk.Main;              use Gtk.Main;
with System;                use System;
with Unchecked_Conversion;
with Unchecked_Deallocation;
with GVD.Types;             use GVD.Types;

package body Process_Proxies is

   procedure Free (Post_Processes : in out Post_Process_Access);
   --  Free the list of post_processes pointed to by
   --  Post_Processes.

   ----------
   -- Free --
   ----------

   procedure Free (Proxy : in out Process_Proxy_Access) is
      procedure Free_Internal is new Unchecked_Deallocation
        (Process_Proxy'Class, Process_Proxy_Access);
      procedure Free_Internal is new Unchecked_Deallocation
        (GNAT.Expect.Process_Descriptor'Class,
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

   ----------------------------
   -- Set_Command_In_Process --
   ----------------------------

   procedure Set_Command_In_Process
     (Proxy      : access Process_Proxy;
      In_Process : Boolean := True) is
   begin
      Proxy.Command_In_Process.all := In_Process;
   end Set_Command_In_Process;

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

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 20) is
   begin
      if Timeout = -1 then
         Expect (Proxy.Descriptor.all, Result, Pattern, Timeout => -1);
      else
         Expect
           (Proxy.Descriptor.all, Result, Pattern, Timeout => Timeout * 50);
      end if;
   end Wait;

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 20) is
   begin
      if Timeout = -1 then
         Expect (Proxy.Descriptor.all, Result, Pattern, Matched, -1);
      else
         Expect (Proxy.Descriptor.all, Result, Pattern, Matched, Timeout * 50);
      end if;
   end Wait;

   procedure Wait
     (Proxy   : access Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : String;
      Timeout : Integer := 20) is
   begin
      Wait (Proxy, Result, Compile (Pattern), Timeout);
   end Wait;

   procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Matched : out GNAT.Regpat.Match_Array;
      Timeout : Integer := 20)
   is
      Tmp        : Boolean;
      Num        : Integer := 1;
      Num_Events : Positive;
      Max_Events : constant := 30;
      --  Limit the number of events to process in one iteration

   begin
      --  We do not use a for loop, so that even if the timeout is 0 we
      --  execute the Expect call at least once.

      loop
         --  In case the external process was killed during the wait.

         if Proxy.Descriptor = null then
            exit;
         end if;

         Expect
           (Proxy.Descriptor.all, Result, Pattern, Matched, Timeout => 10);

         exit when Timeout = 0 or else Num = Timeout;

         Num := Num + 1;

         case Result is
            when Expect_Full_Buffer =>
               --  If the buffer was already full, we simply exit as if there
               --  had been a timeout. This should not be a problem in GVD,
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
                 and then Num_Events <= Max_Events
               loop
                  Tmp := Gtk.Main.Main_Iteration;
                  Num_Events := Num_Events + 1;
               end loop;

            when others =>
               --  It matched, we can simply return.
               exit;
         end case;
      end loop;
   end Wait;

   procedure Wait
     (Proxy   : access Gui_Process_Proxy;
      Result  : out GNAT.Expect.Expect_Match;
      Pattern : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer := 20)
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

   ----------------
   -- TTY_Filter --
   ----------------

   procedure TTY_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Proxy      : System.Address)
   is
      function To_Proxy is new Unchecked_Conversion
        (System.Address, Process_Proxy_Access);
   begin
      if Get_Command_Mode (To_Proxy (Proxy)) /= Internal then
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

   procedure Free (Post_Processes : in out Post_Process_Access) is
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
      Tmp     : Post_Process_Access := Proxy.Post_Processes;

   begin
      --  We must reinitialize the list of post_processes for the proxy
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

   ----------------------
   -- Register_Generic --
   ----------------------

   package body Register_Generic is

      type Data_Access is access Data;
      type Widget_Access is access all Widget'Class;

      type Internal_Data is record
         D    : Data_Access;
         Func : Callback;
         W    : Widget_Access;
      end record;
      type Internal_Data_Access is access all Internal_Data;

      function Convert is new Unchecked_Conversion
        (Internal_Data_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Internal_Data_Access);
      procedure Free is new
        Unchecked_Deallocation (Internal_Data, Internal_Data_Access);
      procedure Free is new Unchecked_Deallocation (Data, Data_Access);

      procedure Internal_Callback (S : System.Address);
      --  Internal function used as a post command.

      -----------------------
      -- Internal_Callback --
      -----------------------

      procedure Internal_Callback (S : System.Address) is
         D : Internal_Data_Access := Convert (S);
      begin
         D.Func (D.W, D.D.all);
         Free (D.D);
         Free (D);
      end Internal_Callback;

      ---------------------------------
      -- Register_Post_Cmd_If_Needed --
      ---------------------------------

      function Register_Post_Cmd_If_Needed
        (Proxy     : access Process_Proxy'Class;
         W         : access Widget'Class;
         Cmd       : Callback;
         User_Data : Data) return Boolean is
      begin
         if Command_In_Process (Proxy) then
            Register_Post_Cmd
              (Proxy, Internal_Callback'Unrestricted_Access,
               Convert (new Internal_Data'
                        (new Data' (User_Data), Cmd, Widget_Access (W))));
            return True;
         end if;

         return False;
      end Register_Post_Cmd_If_Needed;

   end Register_Generic;

end Process_Proxies;
