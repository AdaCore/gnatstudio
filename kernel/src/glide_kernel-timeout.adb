-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                 use Glib;
with Gtk.Main;             use Gtk.Main;

with GNAT.Expect;          use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;      use GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Ada.Exceptions;       use Ada.Exceptions;

with String_Utils;         use String_Utils;
with Traces;               use Traces;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Intl;           use Glide_Intl;

package body Glide_Kernel.Timeout is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Timeout");

   function Process_Cb (Data : Process_Data) return Boolean;
   --  Generic callback for async spawn of processes.

   ----------------
   -- Process_Cb --
   ----------------

   function Process_Cb (Data : Process_Data) return Boolean is
      Name   : String_Access := Data.Name;
      Fd     : Process_Descriptor_Access := Data.Descriptor;
      Result : Expect_Match;
      Status : Integer;

      procedure Cleanup;
      --  Close the process descriptor and free its associated memory

      procedure Cleanup is
      begin
         Close (Fd.all, Status);

         if Data.Exit_Cb = null then
            if Status = 0 then
               Console.Insert
                 (Data.Kernel, -"process terminated successfully");
            else
               Console.Insert
                 (Data.Kernel, -"process exited with status " &
                  Image (Status));
            end if;

         else
            Data.Exit_Cb (Data, Status);
         end if;

         Free (Fd);
         Free (Name);

         if Data.Callback /= null then
            Pop_State (Data.Kernel);
         end if;
      end Cleanup;

   begin
      Expect (Fd.all, Result, "\n", Timeout => 1);

      if Result /= Expect_Timeout then
         Console.Insert (Data.Kernel, Expect_Out (Fd.all), Add_LF => False);
      end if;

      return True;

   exception
      when Process_Died =>
         Console.Insert (Data.Kernel, Expect_Out (Fd.all), Add_LF => False);

         if Data.Callback /= null then
            Data.Callback (Data);
         end if;

         Cleanup;
         return False;

      when E : others =>
         Cleanup;
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Process_Cb;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Kernel    : Kernel_Handle;
      Command   : String;
      Arguments : GNAT.OS_Lib.Argument_List;
      Callback  : Process_Callback := null;
      Exit_Cb   : Exit_Callback := null;
      Name      : String;
      Success   : out Boolean)
   is
      Timeout : constant Guint32 := 50;
      Fd      : Process_Descriptor_Access;
      Id      : Timeout_Handler_Id;

      procedure Spawn
        (Command   : String;
         Arguments : Argument_List;
         Success   : out Boolean);
      --  Launch given command.

      -----------
      -- Spawn --
      -----------

      procedure Spawn
        (Command   : String;
         Arguments : Argument_List;
         Success   : out Boolean)
      is
         Exec : String_Access := Locate_Exec_On_Path (Command);
      begin
         if Exec = null then
            Success := False;
            return;
         end if;

         Console.Insert (Kernel, Command, Add_LF => False);

         for J in Arguments'Range loop
            Console.Insert (Kernel, ' ' & Arguments (J).all, Add_LF => False);
         end loop;

         --  Add end of line after last argument

         Console.Insert (Kernel, "");

         Fd := new TTY_Process_Descriptor;
         Non_Blocking_Spawn (Fd.all, Exec.all, Arguments, Err_To_Out => True);
         Success := True;
         Free (Exec);

      exception
         when Invalid_Process =>
            Success := False;
            Console.Insert (Kernel, -"Invalid command", Mode => Error);
      end Spawn;

   begin
      Push_State (Kernel, Processing);
      Spawn (Command, Arguments, Success);

      if Success then
         Id := Process_Timeout.Add
           (Timeout, Process_Cb'Access,
            (Kernel, Fd, new String'(Name), Callback, Exit_Cb));

         if Callback = null then
            Pop_State (Kernel);
         end if;
      else
         Pop_State (Kernel);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Launch_Process;

end Glide_Kernel.Timeout;
