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
with Glib.Object;          use Glib.Object;
with Glib.Values;

with Gtk.Enums;            use Gtk.Enums;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Main;             use Gtk.Main;

with GNAT.Expect;          use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;      use GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Ada.Exceptions;       use Ada.Exceptions;

with String_Utils;         use String_Utils;
with Traces;               use Traces;

with Glide_Kernel;               use Glide_Kernel;
with Glide_Kernel.Console;       use Glide_Kernel.Console;
with Glide_Kernel.Preferences;   use Glide_Kernel.Preferences;
with Interactive_Consoles;       use Interactive_Consoles;
with Histories;                  use Histories;

with Glide_Intl;           use Glide_Intl;

with Gtkada.MDI;           use Gtkada.MDI;
with Gtkada.Dialogs;       use Gtkada.Dialogs;

package body Glide_Kernel.Timeout is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Timeout");

   type Console_Process_Data is new GObject_Record with record
      Console : Interactive_Console;
      D       : Process_Data;
      Died    : Boolean := False;
      --  Indicates that the process has died.

      Id          : Timeout_Handler_Id;
      Interactive : Boolean := False;
   end record;
   type Console_Process is access all Console_Process_Data'Class;

   package Console_Process_Timeout is new Gtk.Main.Timeout (Console_Process);

   function Process_Cb (Data : Console_Process) return Boolean;
   --  Generic callback for async spawn of processes.

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" event.

   procedure Cleanup (Data : Process_Data);
   --  Close the process descriptor and free its associated memory

   function Data_Handler
     (Input     : in String;
      User_Data : access GObject_Record'Class) return String;
   --  Handler for user input on the console.

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Data : Process_Data) is
      Fd     : Process_Descriptor_Access := Data.Descriptor;
      Name   : String_Access := Data.Name;
      Status : Integer;
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
      Expect (Fd.all, Result, ".+", Timeout => 1);

      if Result /= Expect_Timeout then
         Insert (Data.Console, Expect_Out (Fd.all), Add_LF => False);
         Highlight_Child
           (Find_MDI_Child (Get_MDI (Data.D.Kernel), Data.Console));
      end if;

      return True;

   exception
      when Process_Died =>
         Insert (Data.Console, Expect_Out (Fd.all), Add_LF => False);
         Highlight_Child
           (Find_MDI_Child (Get_MDI (Data.D.Kernel), Data.Console));

         if Data.D.Callback /= null then
            Data.D.Callback (Data.D);
         end if;

         Data.Died := True;
         Cleanup (Data.D);
         Unref (Data);

         if Data.Interactive then
            Enable_Prompt_Display (Data.Console, False);
         end if;

         return False;

      when E : others =>
         Cleanup (Data.D);
         Unref (Data);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Process_Cb;

   ------------------
   -- Data_Handler --
   ------------------

   function Data_Handler
     (Input     : in String;
      User_Data : access GObject_Record'Class) return String
   is
      Console : constant Console_Process := Console_Process (User_Data);
   begin
      if not Console.Died then
         Send (Console.D.Descriptor.all, Input);
      end if;

      return "";

   exception
      when E : others =>
         Timeout_Remove (Console.Id);
         Cleanup (Console.D);
         Unref (Console);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return "";
   end Data_Handler;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Kernel      : Kernel_Handle;
      Command     : String;
      Arguments   : GNAT.OS_Lib.Argument_List;
      Title       : String := "";
      Callback    : Process_Callback := null;
      Exit_Cb     : Exit_Callback := null;
      Name        : String;
      Success     : out Boolean;
      Interactive : Boolean := False)
   is
      Timeout : constant Guint32 := 50;
      Fd      : Process_Descriptor_Access;
      Data    : Console_Process;
      Console : Interactive_Console;
      Child   : MDI_Child;

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

         Glide_Kernel.Console.Insert (Kernel, Command, Add_LF => False);

         for J in Arguments'Range loop
            Glide_Kernel.Console.Insert
              (Kernel, ' ' & Arguments (J).all, Add_LF => False);
         end loop;

         --  Add end of line after last argument

         Glide_Kernel.Console.Insert
           (Kernel, (1 => ASCII.LF), Add_LF => False);

         Fd := new TTY_Process_Descriptor;
         Non_Blocking_Spawn (Fd.all, Exec.all, Arguments, Err_To_Out => True);
         Success := True;
         Free (Exec);

      exception
         when Invalid_Process =>
            Success := False;
            Glide_Kernel.Console.Insert
              (Kernel, -"Invalid command", Mode => Error);
      end Spawn;

   begin
      Push_State (Kernel, Processing);
      Spawn (Command, Arguments, Success);

      if Success then
         Data := new Console_Process_Data;
         Initialize (Data);

         if Interactive then
            Gtk_New
              (Console, "", Data_Handler'Access,
               GObject (Data), Get_Pref (Kernel, Source_Editor_Font),
               History_List => Get_History (Kernel),
               Key          => "external_process",
               Wrap_Mode    => Wrap_Char);
            Set_Max_Length (Get_History (Kernel).all, 100, "external_process");
            Allow_Duplicates
              (Get_History (Kernel).all, "external_process", True, True);

            Data.D := (Kernel, Fd, new String'(Name), Callback, Exit_Cb);

            Object_Return_Callback.Object_Connect
              (Console, "delete_event",
               Delete_Handler'Access,
               GObject (Data),
               After => False);

            Child := Put (Get_MDI (Kernel), Gtk_Widget (Console));
            Set_Focus_Child (Child);
            Set_Dock_Side (Child, Bottom);
            Dock_Child (Child);
            Set_Title (Child, Command, Title);

         else
            Console := Get_Console (Kernel);
            Data.D := (Kernel, Fd, new String'(Name), Callback, Exit_Cb);
         end if;

         Data.Console := Console;
         Data.Interactive := Interactive;

         Data.Id := Console_Process_Timeout.Add
           (Timeout,
            Process_Cb'Access,
            Data);

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
         Unref (Console);
         return False;

      else
         return True;
      end if;

   exception
      when E : others =>
         Timeout_Remove (Console.Id);
         Cleanup (Console.D);
         Unref (Console);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Delete_Handler;

end Glide_Kernel.Timeout;
