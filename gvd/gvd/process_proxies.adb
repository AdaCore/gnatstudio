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

with GNAT.Expect;  use GNAT.Expect;
with GNAT.Regpat;  use GNAT.Regpat;
with Gtk.Main;     use Gtk.Main;
with Unchecked_Deallocation;

package body Process_Proxies is

   ----------
   -- Free --
   ----------

   procedure Free (Proxy : in out Process_Proxy_Access) is
      procedure Free_Internal is new Unchecked_Deallocation
        (Process_Proxy'Class, Process_Proxy_Access);
      procedure Free_Internal is new Unchecked_Deallocation
        (GNAT.Expect.Pipes_Id, GNAT.Expect.Pipes_Id_Access);
   begin
      Free_Internal (Proxy.Pipes);
      Free_Internal (Proxy);
   end Free;

   ---------------
   -- Get_Pipes --
   ---------------

   function Get_Pipes (Proxy : Process_Proxy) return Pipes_Id_Access is
   begin
      return Proxy.Pipes;
   end Get_Pipes;

   ---------------
   -- Set_Pipes --
   ---------------

   procedure Set_Pipes (Proxy : in out Process_Proxy;
                        Pipes : GNAT.Expect.Pipes_Id_Access)
   is
   begin
      Proxy.Pipes := Pipes;
   end Set_Pipes;

   ------------------------
   -- Command_In_Process --
   ------------------------

   function Command_In_Process (Proxy : Process_Proxy) return Boolean is
   begin
      return Proxy.Command_In_Process.all;
   end Command_In_Process;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : GNAT.Regpat.Pattern_Matcher;
                   Timeout : Integer := 20)
   is
   begin
      Proxy.Command_In_Process.all := True;
      if Timeout = -1 then
         Expect (Proxy.Pipes.all, Result, Pattern, Timeout => -1);
      else
         Expect (Proxy.Pipes.all, Result, Pattern, Timeout => Timeout * 50);
      end if;
      Proxy.Command_In_Process.all := False;
   end Wait;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : String;
                   Timeout : Integer := 20)
   is
   begin
      Wait (Proxy, Result, Compile (Pattern), Timeout);
   end Wait;

   ----------
   -- Send --
   ----------

   procedure Send (Proxy : Process_Proxy;
                   Cmd   : String)
   is
   begin
      Send (Proxy.Pipes.all, Cmd);
   end Send;

   ----------------
   -- Expect_Out --
   ----------------

   function Expect_Out (Proxy : Process_Proxy) return String is
   begin
      return Expect_Out (Proxy.Pipes.all);
   end Expect_Out;

   ----------
   -- Wait --
   ----------

   procedure Wait (Proxy   : Gui_Process_Proxy;
                   Result  : out GNAT.Expect.Expect_Match;
                   Pattern : GNAT.Regpat.Pattern_Matcher;
                   Timeout : Integer := 20)
   is
      Tmp    : Boolean;
      Num    : Integer := 1;
   begin
      Proxy.Command_In_Process.all := True;

      --  We do not use a for loop, so that even if the timeout is 0 we
      --  execute the Expect call at least once.

      loop

         --  In case the external process was killed during the wait.

         if Proxy.Pipes = null then
            exit;
         end if;

         Expect (Proxy.Pipes.all, Result, Pattern, Timeout => 50);

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

               while Gtk.Main.Events_Pending loop
                  Tmp := Gtk.Main.Main_Iteration;
               end loop;

            when others =>
               --  It matched, we can simply return.
               exit;
         end case;
      end loop;

      Proxy.Command_In_Process.all := False;
   end Wait;

end Process_Proxies;
