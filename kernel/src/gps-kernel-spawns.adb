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

with Spawn.Process_Listeners;
with Spawn.Processes;
with Spawn.String_Vectors;

with VSS.Strings.Conversions;

with Commands;

package body GPS.Kernel.Spawns is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.SPAWN");
   Me_IO : constant Trace_Handle := Create
     ("GPS.KERNEL.SPAWN_IO", Off);

   type Monitor_Command is new Commands.Root_Command
     and Spawn.Process_Listeners.Process_Listener with
      record
         Name          : VSS.Strings.Virtual_String;
         Label         : VSS.Strings.Virtual_String;
         Process       : Spawn.Processes.Process;
         Output_Parser : GPS.Tools_Output.Tools_Output_Parser_Access;
         Failed        : Boolean := False;
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

   overriding procedure Interrupt (Self : in out Monitor_Command) is
   begin
      Self.Process.Terminate_Process;
   end Interrupt;

   overriding procedure Primitive_Free (Self : in out Monitor_Command) is
   begin
      Self.Process.Kill_Process;
   end Primitive_Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Monitor_Command) return Commands.Command_Return_Type is
   begin
      case Self.Process.Status is
         when Spawn.Not_Running =>
            return (if Self.Failed then Commands.Failure
                    else Commands.Success);

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
   begin
      loop
         declare
            Data    : Ada.Streams.Stream_Element_Array (1 .. 512);
            Text    : String (1 .. 512) with Import, Address => Data'Address;
            Last    : Ada.Streams.Stream_Element_Count;
            Success : Boolean := True;

         begin
            Self.Process.Read_Standard_Error (Data, Last, Success);

            exit when Last < Data'First;

            Me_IO.Trace (Text (1 .. Positive (Last)));
            Self.Output_Parser.Parse_Standard_Output
              (Text (1 .. Positive (Last)), Self'Unchecked_Access);
            --  FIXME: use stderr! But now parsers expect whole text on
            --  stdout, so send it there for now.
         end;
      end loop;
   end Standard_Error_Available;

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available
     (Self : in out Monitor_Command)
   is
      use type Ada.Streams.Stream_Element_Count;
   begin
      loop
         declare
            Data    : Ada.Streams.Stream_Element_Array (1 .. 512);
            Text    : String (1 .. 512) with Import, Address => Data'Address;
            Last    : Ada.Streams.Stream_Element_Count;
            Success : Boolean := True;

         begin
            Self.Process.Read_Standard_Output (Data, Last, Success);

            exit when Last < Data'First;

            Me_IO.Trace (Text (1 .. Positive (Last)));
            Self.Output_Parser.Parse_Standard_Output
              (Text (1 .. Positive (Last)), Self'Unchecked_Access);
         end;
      end loop;
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
   end Exception_Occurred;

   --------------
   -- Finished --
   --------------

   overriding procedure Finished
     (Self        : in out Monitor_Command;
      Exit_Status : Spawn.Process_Exit_Status;
      Exit_Code   : Spawn.Process_Exit_Code)
   is
      use all type Spawn.Process_Exit_Status;
      use type Spawn.Process_Exit_Code;

      Integer_Last : constant Spawn.Process_Exit_Code :=
        Spawn.Process_Exit_Code (Integer'Last);

      Int_Code : constant Integer :=
        (if Exit_Code in 0 .. Integer_Last then Integer (Exit_Code)
         else -Integer (Spawn.Process_Exit_Code'Last - Exit_Code) - 1);
   begin
      Self.Failed := Exit_Status = Crash;
      Me_IO.Trace ("Finished: " & Exit_Status'Image & Exit_Code'Image);
      Self.Output_Parser.End_Of_Stream (Int_Code, Self'Unchecked_Access);
   end Finished;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Command_Name  : VSS.Strings.Virtual_String;
      Exec          : String;
      Arg_List      : GNATCOLL.Arg_Lists.Arg_List;
      Env           : Spawn.Environments.Process_Environment;
      Directory     : String;
      Use_Pipes     : Boolean;
      Output_Parser : GPS.Tools_Output.Tools_Output_Parser_Access;
      Command       : out Commands.Command_Access)
   is
      Obj : constant Monitor_Command_Access := new Monitor_Command'
        (Commands.Root_Command with
         Name          => Command_Name,
         Output_Parser => Output_Parser,
         others        => <>);

      Arguments : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      if Exec = "" then
         return;
      elsif not Use_Pipes then
         Obj.Process.Set_Standard_Input_PTY;
         Obj.Process.Set_Standard_Output_PTY;
         Obj.Process.Set_Standard_Error_PTY;
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
        (Me, "Spawning " & GNATCOLL.Arg_Lists.To_Display_String (Arg_List));

      Obj.Process.Start;
      Command := Commands.Command_Access (Obj);
   end Launch_Process;

end GPS.Kernel.Spawns;
