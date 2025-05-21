------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Exceptions;
with Ada.Streams;

with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Gtkada.Dialogs;
with Gtk.Handlers;

with GPS.Intl;                   use GPS.Intl;
with GUI_Utils;

with Spawn.Process_Listeners;
with Spawn.Processes;
with Spawn.String_Vectors;

with VSS.Strings.Conversions;

with Commands;

package body GPS.Kernel.Spawns is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.SPAWN", Off);
   --  Disable Spawn as the default API to launch external processes
   --  until we solve the remaining issues (#538 and #495).

   Me_IO : constant Trace_Handle := Create
     ("GPS.KERNEL.SPAWN_IO", Off);

   Buffer_Size : constant := 2048;

   type Monitor_Command is new Commands.Root_Command
     and Spawn.Process_Listeners.Process_Listener with
      record
         Name          : VSS.Strings.Virtual_String;
         Label         : VSS.Strings.Virtual_String;
         Process       : Spawn.Processes.Process;
         Console       : Interactive_Consoles.Interactive_Console;
         Delete_Id     : Gtk.Handlers.Handler_Id;
         --  Signals connecting the gtk widget to the underlying process
         Output_Parser : GPS.Tools_Output.Tools_Output_Parser_Access;
         Finished      : Boolean := False;  --  Process field value is invalid
         Failed        : Boolean := False;
         Read_Output   : Boolean := False;
         Read_Error    : Boolean := False;
      end record;

   type Monitor_Command_Access is access all Monitor_Command'Class;
   --  Command that can be used to monitor an external process through the task
   --  manager, and make it interruptible by users. The output handling is done
   --  by output parser chain.

   overriding procedure Interrupt (Self : in out Monitor_Command);
   overriding procedure Primitive_Free (Self : in out Monitor_Command);
   overriding function Execute
     (Self : access Monitor_Command) return Commands.Command_Return_Type;
   overriding function Name (Self : access Monitor_Command) return String
     is (VSS.Strings.Conversions.To_UTF_8_String (Self.Name));
   overriding function Get_Label
     (Self : access Monitor_Command) return String is
       (if Self.Label.Is_Empty then Name (Self)
        else VSS.Strings.Conversions.To_UTF_8_String (Self.Label));

   overriding procedure Set_Label
     (Self : in out Monitor_Command;
      To   : String);

   overriding procedure Standard_Output_Available
     (Self : in out Monitor_Command);

   overriding procedure Standard_Error_Available
     (Self : in out Monitor_Command);

   overriding procedure Finished
    (Self        : in out Monitor_Command;
     Exit_Status : Spawn.Process_Exit_Status;
     Exit_Code   : Spawn.Process_Exit_Code);

   overriding procedure Error_Occurred
     (Self : in out Monitor_Command;
      Error  : Integer);

   overriding procedure Exception_Occurred
     (Self  : in out Monitor_Command;
      Error : Ada.Exceptions.Exception_Occurrence);

   package Monitor_Callbacks is new Gtk.Handlers.User_Return_Callback
     (Interactive_Consoles.Interactive_Console_Record,
      Boolean,
      Monitor_Command_Access);

   function Input_Handler
     (Console   : access Interactive_Consoles.Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String;

   function Delete_Handler
     (Console : access Interactive_Consoles.Interactive_Console_Record'Class;
      Command : Monitor_Command_Access) return Boolean;

   function To_Exit_Code (Code : Spawn.Process_Exit_Code) return Integer;
   --  This code convert unsigned Process_Exit_Code value to Integer.
   --  We can't just do Integer (Code), because we will get Constraint_Error
   --  if Code >= 2**31.

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Self : in out Monitor_Command) is
      use all type Spawn.Process_Status;
   begin
      if not Self.Finished and then Self.Process.Status = Running then
         Self.Process.Terminate_Process;
      end if;
   end Interrupt;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Self : in out Monitor_Command) is
      use all type Spawn.Process_Status;
   begin
      if not Self.Finished and then Self.Process.Status = Running then
         Self.Process.Kill_Process;
      end if;
   end Primitive_Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Monitor_Command) return Commands.Command_Return_Type is
   begin
      if Self.Read_Output then
         Self.Standard_Output_Available;  --  process more stdout data

         return Commands.Execute_Again;
      elsif Self.Read_Error then
         Self.Standard_Error_Available;  --  process more stdout data

         return Commands.Execute_Again;
      elsif Self.Finished then

         return (if Self.Failed then Commands.Failure
                 else Commands.Success);
      end if;

      case Self.Process.Status is
         when Spawn.Not_Running =>

            declare
               This : Commands.Command_Access :=
                 Commands.Command_Access (Self);

               Result : constant Commands.Command_Return_Type :=
                 (if Self.Failed then Commands.Failure
                    else Commands.Success);
            begin
               Commands.Unref (This);
               return Result;
            end;

         when Spawn.Starting | Spawn.Running =>
            return Commands.Execute_Again;
      end case;
   end Execute;

   overriding procedure Set_Label
     (Self : in out Monitor_Command;
      To   : String) is
   begin
      Self.Label := VSS.Strings.Conversions.To_Virtual_String (To);
   end Set_Label;

   ------------------------------
   -- Standard_Error_Available --
   ------------------------------

   overriding procedure Standard_Error_Available
     (Self : in out Monitor_Command)
   is
      use type Ada.Streams.Stream_Element_Count;
      Data    : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
      Text    : String (1 .. Buffer_Size) with Import, Address => Data'Address;
      Last    : Ada.Streams.Stream_Element_Count;
      Success : Boolean := True;

   begin
      Self.Process.Read_Standard_Error (Data, Last, Success);

      Self.Read_Error := Success and Last >= Data'First;
      --  Read stderr again if any data

      if Self.Read_Error then
         Me_IO.Trace (Text (1 .. Positive (Last)));
         Self.Output_Parser.Parse_Standard_Output
           (Text (1 .. Positive (Last)), Self'Unchecked_Access);
         --  FIXME: use stderr! But now parsers expect whole text on
         --  stdout, so send it there for now.
      end if;
   end Standard_Error_Available;

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available
     (Self : in out Monitor_Command)
   is
      use type Ada.Streams.Stream_Element_Count;
      Data    : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
      Text    : String (1 .. Buffer_Size) with Import, Address => Data'Address;
      Last    : Ada.Streams.Stream_Element_Count;
      Success : Boolean := True;

   begin
      Self.Process.Read_Standard_Output (Data, Last, Success);

      Self.Read_Output := Success and Last >= Data'First;
      --  Read stdout again if any data

      if Self.Read_Output then
         Me_IO.Trace (Text (1 .. Positive (Last)));
         Self.Output_Parser.Parse_Standard_Output
           (Text (1 .. Positive (Last)), Self'Unchecked_Access);
      end if;
   end Standard_Output_Available;

   --------------------
   -- Error_Occurred --
   --------------------

   overriding procedure Error_Occurred
    (Self : in out Monitor_Command;
     Error  : Integer) is
   begin
      Self.Failed := True;
      Me.Trace ("Error in spawn:" & Error'Image);
   end Error_Occurred;

   overriding procedure Exception_Occurred
     (Self  : in out Monitor_Command;
      Error : Ada.Exceptions.Exception_Occurrence) is
   begin
      Me.Trace (Error);
      Self.Failed := True;
      Self.Finished := True;
   end Exception_Occurred;

   ------------------
   -- To_Exit_Code --
   ------------------

   function To_Exit_Code (Code : Spawn.Process_Exit_Code) return Integer is
      use type Spawn.Process_Exit_Code;

      Integer_Last : constant Spawn.Process_Exit_Code :=
        Spawn.Process_Exit_Code (Integer'Last);

      Result : constant Integer :=
        (if Code in 0 .. Integer_Last then Integer (Code)
         else -Integer (Spawn.Process_Exit_Code'Last - Code) - 1);
   begin
      return Result;
   end To_Exit_Code;

   --------------
   -- Finished --
   --------------

   overriding procedure Finished
     (Self        : in out Monitor_Command;
      Exit_Status : Spawn.Process_Exit_Status;
      Exit_Code   : Spawn.Process_Exit_Code)
   is
      use all type Spawn.Process_Exit_Status;
      use type Interactive_Consoles.Interactive_Console;

   begin
      --  Complete reading from stdout
      while Self.Read_Output loop
         Self.Standard_Output_Available;
      end loop;

      --  Complete reading from stderr
      while Self.Read_Error loop
         Self.Standard_Error_Available;
      end loop;

      Self.Failed := Exit_Status = Crash;
      Self.Finished := True;
      Me_IO.Trace ("Finished: " & Exit_Status'Image & Exit_Code'Image);
      Self.Output_Parser.End_Of_Stream
        (To_Exit_Code (Exit_Code), Self'Unchecked_Access);

      if Self.Console /= null then
         Gtk.Handlers.Disconnect (Self.Console, Self.Delete_Id);
         Self.Console.Set_Command_Handler (null, System.Null_Address);
      end if;
   end Finished;

   --------------------
   -- Delete_Handler --
   --------------------

   function Delete_Handler
     (Console : access Interactive_Consoles.Interactive_Console_Record'Class;
      Command : Monitor_Command_Access) return Boolean
   is
      use Gtkada.Dialogs;
      use all type Spawn.Process_Status;

      Button  : Message_Dialog_Buttons;
   begin
      if not Command.Finished and then Command.Process.Status = Running then

         Button := GUI_Utils.GPS_Message_Dialog
           (-"The process attached to this window" & ASCII.LF
            & (-"is still active, do you want to kill it ?"),
            Confirmation,
            Button_Yes or Button_No,
            Button_Yes,
            Parent => Console.Kernel.Get_Main_Window);

         if Button = Button_Yes then

            Command.Interrupt;

            return False;
         end if;

         return True;
      else
         return False;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Delete_Handler;

   -------------------
   -- Input_Handler --
   -------------------

   function Input_Handler
     (Console   : access Interactive_Consoles.Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String
   is
      pragma Unreferenced (Console);
      use all type Spawn.Process_Status;

      Last : Ada.Streams.Stream_Element_Offset := Input'Length;
      Data : Ada.Streams.Stream_Element_Array (1 .. Last)
        with Import, Address => Input'Address;
      Ok   : Boolean := True;

      Command : Monitor_Command
        with Import, Address => User_Data;
   begin
      if not Command.Finished and then Command.Process.Status = Running then
         Command.Process.Write_Standard_Input (Data, Last, Ok);
      end if;

      return "";
   end Input_Handler;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Command_Name  : VSS.Strings.Virtual_String;
      Console       : Interactive_Consoles.Interactive_Console;
      Exec          : String;
      Arg_List      : GNATCOLL.Arg_Lists.Arg_List;
      Env           : Spawn.Environments.Process_Environment;
      Directory     : String;
      Use_Pipes     : Boolean;
      Output_Parser : GPS.Tools_Output.Tools_Output_Parser_Access;
      Command       : out Commands.Command_Access)
   is
      use type Interactive_Consoles.Interactive_Console;
      use type Spawn.Process_Status;

      Obj : constant Monitor_Command_Access := new Monitor_Command'
        (Commands.Root_Command with
         Name          => Command_Name,
         Output_Parser => Output_Parser,
         others        => <>);

      Arguments : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      if Exec = "" then
         return;

      elsif Console /= null
        or else not Use_Pipes
      then
         --  We're displaying the process output in a Console: set
         --  stdout and stderr to use a pseudo terminal - this allows
         --  the program to use ncurses, colors, etc.
         Obj.Process.Set_Standard_Input_PTY;
         Obj.Process.Set_Standard_Output_PTY;
         Obj.Process.Set_Standard_Error_PTY;
      end if;

      if Console /= null then
         Trace (Me_IO, "Connect the command_handler to the console");
         Console.Set_Command_Handler (Input_Handler'Access, Obj.all'Address);

         Obj.Console := Console;
         Obj.Delete_Id := Monitor_Callbacks.Connect
           (Console,
            Gtk.Widget.Signal_Delete_Event,
            Monitor_Callbacks.To_Marshaller (Delete_Handler'Access),
            Obj);
      end if;

      for J in 1 .. GNATCOLL.Arg_Lists.Args_Length (Arg_List) loop
         Arguments.Append (GNATCOLL.Arg_Lists.Nth_Arg (Arg_List, J));
      end loop;

      Obj.Process.Set_Arguments (Arguments);
      Obj.Process.Set_Program (Exec);
      Obj.Process.Set_Environment (Env);
      Obj.Process.Set_Working_Directory (Directory);
      Obj.Process.Set_Listener
        (Spawn.Process_Listeners.Process_Listener_Access (Obj));

      Trace
        (Me_IO, "Spawning " & GNATCOLL.Arg_Lists.To_Display_String (Arg_List));

      Obj.Ref;  --  Keep command alive until process finishes
      Obj.Process.Start;

      if Obj.Process.Status = Spawn.Not_Running then
         --  The proccess can't be started, call End_Of_Stream
         --  with Exit_Code to print message in the Message view
         --  to inform a user that process is not started.

         Obj.Output_Parser.End_Of_Stream
           (To_Exit_Code (Obj.Process.Exit_Code), Obj);
      end if;

      Command := Commands.Command_Access (Obj);
   end Launch_Process;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled return Boolean is
     (Me.Is_Active);

end GPS.Kernel.Spawns;
