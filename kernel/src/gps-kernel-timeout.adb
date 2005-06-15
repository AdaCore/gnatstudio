-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with Config;               use Config;

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Values;

with Gtk.Main;             use Gtk.Main;

with GNAT.Expect;          use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;      use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;          use GNAT.Regpat;

with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with String_Utils;         use String_Utils;
with Traces;               use Traces;

with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Console;       use GPS.Kernel.Console;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with Interactive_Consoles;       use Interactive_Consoles;
with System;                     use System;

with GPS.Intl;           use GPS.Intl;

with Gtkada.MDI;           use Gtkada.MDI;
with Gtkada.Dialogs;       use Gtkada.Dialogs;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Unchecked_Deallocation;

package body GPS.Kernel.Timeout is

   type Console_Process_Data is new GObject_Record with record
      Console   : Interactive_Console;
      Delete_Id : Gtk.Handlers.Handler_Id;

      Expect_Regexp : GNAT.Expect.Pattern_Matcher_Access;

      D       : Process_Data;
      Died    : Boolean := False;
      --  Indicates that the process has died.

      Id          : Timeout_Handler_Id;
   end record;
   type Console_Process is access all Console_Process_Data'Class;

   package Console_Process_Timeout is new Gtk.Main.Timeout (Console_Process);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

   function Process_Cb (Data : Console_Process) return Boolean;
   --  Generic callback for async spawn of processes.

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" event.

   procedure Cleanup (Data : Process_Data);
   --  Close the process descriptor and free its associated memory

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : in String;
      User_Data : System.Address) return String;
   --  Handler for user input on the console.

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Data : Process_Data) is
      Fd     : Process_Descriptor_Access := Data.Descriptor;
      Status : Integer;
   begin
      Close (Fd.all, Status);

      if Data.Exit_Cb = null then
         if Status = 0 then
            Console.Insert
              (Data.Kernel, ASCII.LF & (-"process terminated successfully"));
         else
            Console.Insert
              (Data.Kernel, ASCII.LF & (-"process exited with status ") &
                 Image (Status));
         end if;

      else
         Data.Exit_Cb (Data, Status);
      end if;

      Free (Fd);

      Pop_State (Data.Kernel);
   end Cleanup;

   ----------------
   -- Process_Cb --
   ----------------

   function Process_Cb (Data : Console_Process) return Boolean is
      Fd     : Process_Descriptor_Access;
      Result : Expect_Match;

   begin
      if Data = null or else Data.Died then
         return False;
      end if;

      Fd := Data.D.Descriptor;
      Expect (Fd.all, Result, Data.Expect_Regexp.all, Timeout => 1);

      if Result /= Expect_Timeout then
         declare
            Output : constant String := Strip_CR (Expect_Out (Fd.all));
         begin
            if Data.Console /= null then
               Insert (Data.Console, Output, Add_LF => False);

               --  ??? This might be costly, we could cache this MDI Child
               Highlight_Child
                 (Find_MDI_Child (Get_MDI (Data.D.Kernel), Data.Console));
            end if;

            if Data.D.Callback /= null then
               Data.D.Callback (Data.D, Output);
            end if;
         end;
      end if;

      return True;

   exception
      when Process_Died =>
         declare
            Output : constant String := Strip_CR (Expect_Out (Fd.all));
         begin
            if Data.D.Callback /= null then
               Data.D.Callback (Data.D, Output);
            end if;

            if Data.Console /= null then
               --  Display all remaining output

               Insert (Data.Console, Output, Add_LF => False);
               Highlight_Child
                 (Find_MDI_Child (Get_MDI (Data.D.Kernel), Data.Console));

               if Data.Console /= Get_Console (Data.D.Kernel) then
                  Enable_Prompt_Display (Data.Console, False);
               end if;
            end if;
         end;

         if Data.Delete_Id.Signal /= Null_Signal_Id then
            Gtk.Handlers.Disconnect (Data.Console, Data.Delete_Id);
         end if;

         Data.Died := True;
         Cleanup (Data.D);
         Unchecked_Free (Data.Expect_Regexp);
         Unref (Data);

         return False;

      when E : others =>
         Cleanup (Data.D);
         Unchecked_Free (Data.Expect_Regexp);
         Unref (Data);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Process_Cb;

   ------------------
   -- Data_Handler --
   ------------------

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : in String;
      User_Data : System.Address) return String
   is
      pragma Unreferenced (Console);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Console_Process);
      Process : constant Console_Process := Convert (User_Data);
   begin
      if not Process.Died then
         Send (Process.D.Descriptor.all, Input);
      end if;

      return "";

   exception
      when E : others =>
         Timeout_Remove (Process.Id);
         Unchecked_Free (Process.Expect_Regexp);
         Cleanup (Process.D);
         Unref (Process);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return "";
   end Data_Handler;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Kernel        : Kernel_Handle;
      Command       : String;
      Arguments     : GNAT.OS_Lib.Argument_List;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Callback      : Output_Callback := null;
      Exit_Cb       : Exit_Callback := null;
      Success       : out Boolean;
      Show_Command  : Boolean := True;
      Callback_Data : System.Address := System.Null_Address;
      Line_By_Line  : Boolean := False;
      Directory     : String := "";
      Fd            : out GNAT.Expect.Process_Descriptor_Access)
   is
      Timeout : constant Guint32 := 50;
      Data    : Console_Process;

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
         Exec    : String_Access := Locate_Exec_On_Path (Command);
         Old_Dir : String_Access;
      begin
         if Exec = null then
            Success := False;
            GPS.Kernel.Console.Insert
              (Kernel, -"Executable not found on PATH: " & Command,
               Mode => Error);
            return;
         end if;

         if Data.Console /= null and then Show_Command then
            Insert (Data.Console, Command, Add_LF => False);
            for J in Arguments'Range loop
               Insert
                 (Data.Console, ' ' & Arguments (J).all, Add_LF => False);
            end loop;

            --  Add end of line after last argument

            Insert (Data.Console, "", Add_LF => True);
         end if;

         Fd := new TTY_Process_Descriptor;

         if Directory /= "" then
            Old_Dir := new String'(Get_Current_Dir);
            Change_Dir (Directory);
         end if;

         if Host = Windows then
            --  ??? Should remove this kludge, the one in
            --  commands-external.adb, and the one in
            --  GVD.Proc_Utils.Open_Processes

            declare
               Real_Args : GNAT.OS_Lib.Argument_List (1 .. 2);
            begin
               Real_Args (1) := new String'("/c");
               Real_Args (2) := new String'(Command);

               Non_Blocking_Spawn
                 (Fd.all,
                  "cmd",
                  Real_Args & Arguments,
                  Err_To_Out => True);

               GNAT.OS_Lib.Free (Real_Args (1));
               GNAT.OS_Lib.Free (Real_Args (2));
            end;
         else
            Non_Blocking_Spawn
              (Fd.all, Exec.all, Arguments, Err_To_Out => True);
         end if;

         if Directory /= "" then
            Change_Dir (Old_Dir.all);
            Free (Old_Dir);
         end if;

         Success := True;
         Free (Exec);

      exception
         when Invalid_Process =>
            Success := False;
            GPS.Kernel.Console.Insert
              (Kernel, -"Invalid command", Mode => Error);
      end Spawn;

   begin
      Push_State (Kernel, Processing);

      --  Data is freed in Process_Cb
      Data := new Console_Process_Data;
      Initialize (Data);

      if Console /= null then
         Set_Command_Handler (Console, Data_Handler'Access, Data.all'Address);
         Data.Delete_Id := Object_Return_Callback.Object_Connect
           (Console, "delete_event",
            Delete_Handler'Access,
            GObject (Data),
            After => False);
      end if;

      Data.Console := Console;

      Spawn (Command, Arguments, Success);

      if Success then
         --  Precompile the regular expression for more efficiency
         if Line_By_Line then
            Data.Expect_Regexp := new Pattern_Matcher'(Compile ("^.*?\n"));
         else
            Data.Expect_Regexp := new Pattern_Matcher'
              (Compile (".*$", Single_Line));
         end if;

         Data.D       := (Kernel, Fd, Callback, Exit_Cb, Callback_Data);
         Data.Delete_Id.Signal := Null_Signal_Id;
         Data.Id      :=
           Console_Process_Timeout.Add (Timeout, Process_Cb'Access, Data);
      else
         Unref (Data);
         Pop_State (Kernel);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Launch_Process;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Kernel        : Kernel_Handle;
      Command       : String;
      Arguments     : GNAT.OS_Lib.Argument_List;
      Console       : Interactive_Consoles.Interactive_Console := null;
      Callback      : Output_Callback := null;
      Exit_Cb       : Exit_Callback := null;
      Success       : out Boolean;
      Show_Command  : Boolean := True;
      Callback_Data : System.Address := System.Null_Address;
      Line_By_Line  : Boolean := False;
      Directory     : String := "")
   is
      Fd : Process_Descriptor_Access;
   begin
      Launch_Process
        (Kernel, Command, Arguments, Console, Callback, Exit_Cb, Success,
         Show_Command, Callback_Data, Line_By_Line, Directory, Fd);
   end Launch_Process;

   --------------------
   -- Delete_Handler --
   --------------------

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);

      Console : constant Console_Process := Console_Process (Object);
      Button  : Message_Dialog_Buttons;
   begin
      if Console.Died then
         Timeout_Remove (Console.Id);
         Unref (Console);
         return False;
      end if;

      Button := Message_Dialog
        (-"The process attached to this window" & ASCII.LF
          & (-"is still active, do you want to kill it ?"),
         Confirmation,
         Button_Yes or Button_No,
         Button_Yes);

      if Button = Button_Yes then
         Timeout_Remove (Console.Id);
         Cleanup (Console.D);
         Unchecked_Free (Console.Expect_Regexp);
         Unref (Console);
         return False;

      else
         return True;
      end if;

   exception
      when E : others =>
         Timeout_Remove (Console.Id);
         Unchecked_Free (Console.Expect_Regexp);
         Cleanup (Console.D);
         Unref (Console);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Delete_Handler;

end GPS.Kernel.Timeout;
