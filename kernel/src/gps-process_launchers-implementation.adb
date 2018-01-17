------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

with GPS.Kernel.Timeout;               use GPS.Kernel.Timeout;
with GPS.Scripts.Commands;             use GPS.Scripts.Commands;
with GPS.Messages_Windows;             use GPS.Messages_Windows;
with Interactive_Consoles;             use Interactive_Consoles;
with GPS.Kernel;                       use GPS.Kernel;

package body GPS.Process_Launchers.Implementation is

   type Build_Callback_Data is new External_Process_Data with record
      Output_Parser  : Tools_Output_Parser_Access;
   end record;
   type Build_Callback_Data_Access is access all Build_Callback_Data'Class;
   overriding procedure Free (Data : in out Build_Callback_Data);
   overriding procedure On_Output
     (Self    : not null access Build_Callback_Data;
      Command : not null access Root_Command'Class;
      Output  : String);
   overriding procedure On_Exit
     (Self    : not null access Build_Callback_Data;
      Command : not null access Root_Command'Class);

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

   overriding procedure Free (Data : in out Build_Callback_Data) is
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
      Console : Interactive_Consoles.Interactive_Console;
      Result : Scheduled_Command_Access;
      Data   : constant Build_Callback_Data_Access := new Build_Callback_Data'
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
         Show_Exit_Status     => False,
         Scheduled            => Result,
         Show_In_Task_Manager => Show_In_Task_Manager,
         Name_In_Task_Manager => Name_In_Task_Manager,
         Block_Exit           => Block_Exit);

      Created_Command := Command_Access (Result);
   end Launch_Process_In_Background;

end GPS.Process_Launchers.Implementation;
