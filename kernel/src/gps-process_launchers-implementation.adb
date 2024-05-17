------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2013-2023, AdaCore                  --
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

with VSS.Strings.Conversions;

with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel.Timeout;               use GPS.Kernel.Timeout;
with GPS.Kernel.Spawns;
with GPS.Scripts.Commands;             use GPS.Scripts.Commands;
with GPS.Messages_Windows;             use GPS.Messages_Windows;
with Interactive_Consoles;             use Interactive_Consoles;
with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Task_Manager;
with GPS.Kernel.Remote;
with UTF8_Utils;                       use UTF8_Utils;

package body GPS.Process_Launchers.Implementation is

   type Build_Callback_Data is new External_Process_Data with record
      Output_Parser  : Tools_Output_Parser_Access;
   end record;
   type Build_Callback_Data_Access is access all Build_Callback_Data'Class;
   overriding procedure Free
     (Data    : in out Build_Callback_Data;
      Partial : Boolean := False);
   overriding procedure On_Output
     (Self    : not null access Build_Callback_Data;
      Command : not null access Root_Command'Class;
      Output  : String);
   overriding procedure On_Exit
     (Self    : not null access Build_Callback_Data;
      Command : not null access Root_Command'Class);

   Queue_Id : Natural := 0;

   ---------------
   -- On_Output --
   ---------------

   overriding procedure On_Output
     (Self    : not null access Build_Callback_Data;
      Command : not null access Root_Command'Class;
      Output  : String) is
   begin
      if Self.Output_Parser /= null then
         Self.Output_Parser.Parse_Standard_Output (Output, Command);
      end if;
   end On_Output;

   -------------
   -- On_Exit --
   -------------

   overriding procedure On_Exit
     (Self    : not null access Build_Callback_Data;
      Command : not null access Root_Command'Class) is
   begin
      if Self.Output_Parser /= null then
         Self.Output_Parser.End_Of_Stream (Self.Exit_Status, Command);
      end if;
   end On_Exit;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Data    : in out Build_Callback_Data;
      Partial : Boolean := False) is
   begin
      Free (Data.Output_Parser);
   end Free;

   --------------------
   -- Launch_Process --
   --------------------

   overriding procedure Launch_Process
     (Launcher             : access GPS_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Messages_Windows.Abstract_Messages_Window_Access;
      Success              : out Boolean)
   is
      Console : Interactive_Consoles.Interactive_Console;
      Result  : Scheduled_Command_Access;
      Data    : constant Build_Callback_Data_Access := new Build_Callback_Data'
        (External_Process_Data with Output_Parser => Output_Parser);
   begin
      if Show_Command_To /= null then
         Console := Console_Messages_Window (Show_Command_To.all)
           .Get_Interactive_Console;
      end if;

      GPS.Kernel.Timeout.Launch_Process
        (Kernel               => Kernel_Handle (Launcher.Kernel),
         CL                   => CL,
         Server               => Server,
         Success              => Success,
         Use_Ext_Terminal     => False,
         Console              => Console,
         Show_Command         => Show_Command_To /= null,
         Show_Output          => False,
         Data                 => Data,
         Line_By_Line         => False,
         Directory            => Directory,
         Synchronous          => True,
         Show_Exit_Status     => False,
         Scheduled            => Result);
   end Launch_Process;

   ----------------------------------
   -- Launch_Process_In_Background --
   ----------------------------------

   overriding procedure Launch_Process_In_Background
     (Launcher             : access GPS_Process_Launcher_Record;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Output_Parser        : GPS.Tools_Output.Tools_Output_Parser_Access;
      Show_Command_To      : Messages_Windows.Abstract_Messages_Window_Access;
      Success              : out Boolean;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Block_Exit           : Boolean := True;
      Created_Command      : out Command_Access)
   is
      Console : constant Interactive_Consoles.Interactive_Console :=
        (if Show_Command_To = null then null
         else Console_Messages_Window (Show_Command_To.all)
                .Get_Interactive_Console);

      Exec : constant String := GPS.Kernel.Remote.Check_Exec
        (Server, Get_Command (CL));

      Kernel  : constant Kernel_Handle := Kernel_Handle (Launcher.Kernel);
      Result  : Scheduled_Command_Access;
      Data    : Build_Callback_Data_Access;
   begin
      if Is_Local (Server) and then GPS.Kernel.Spawns.Is_Enabled then
         if Exec = "" then
            Success := False;

            Insert
              (Kernel,
               -"Could not locate executable on path: " &
                 Unknown_To_UTF8 (Get_Command (CL)),
               Mode => GPS.Kernel.Error);

            return;

         elsif Show_Command_To /= null then
            Console.Insert_With_Links (To_Display_String (CL), Add_LF => True);
         end if;

         GPS.Kernel.Spawns.Launch_Process
           (Command_Name  =>
              VSS.Strings.Conversions.To_Virtual_String
                (if Name_In_Task_Manager = "" then Get_Command (CL)
                 else Name_In_Task_Manager),
            Exec          => Exec,
            Arg_List      => CL,
            Env           => Kernel.Get_Original_Environment,
            Directory     => Directory.Display_Full_Name,
            Use_Pipes     => True,
            Output_Parser => Output_Parser,
            Command       => Created_Command);

         Queue_Id := Queue_Id + 1;

         GPS.Kernel.Task_Manager.Launch_Background_Command
           (Kernel,
            Created_Command,
            Active            => True,
            Show_Bar          => Show_In_Task_Manager,
            Queue_Id          => "gps-kernel-spawn" & Queue_Id'Image,
            Block_Exit        => Block_Exit,
            Start_Immediately => False);

         Success := True;
      else
         Data := new Build_Callback_Data'
           (External_Process_Data with Output_Parser => Output_Parser);

         GPS.Kernel.Timeout.Launch_Process
           (Kernel               => Kernel,
            CL                   => CL,
            Server               => Server,
            Success              => Success,
            Use_Ext_Terminal     => False,
            Console              => Console,
            Show_Command         => Show_Command_To /= null,
            Show_Output          => False,
            Data                 => Data,
            Line_By_Line         => False,
            Directory            => Directory,
            Show_Exit_Status     => False,
            Scheduled            => Result,
            Show_In_Task_Manager => Show_In_Task_Manager,
            Name_In_Task_Manager => Name_In_Task_Manager,
            Block_Exit           => Block_Exit);

         Created_Command := Command_Access (Result);
      end if;
   end Launch_Process_In_Background;

end GPS.Process_Launchers.Implementation;
