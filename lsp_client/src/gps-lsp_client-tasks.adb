------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2021, AdaCore                   --
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

with Commands;
with GPS.Kernel.Task_Manager;

package body GPS.LSP_Client.Tasks is

   -------------------------
   -- Monitoring activity --
   -------------------------

   --  This is a command that's used to indicate activity that requests
   --  are being processed in the task manager.

   type Language_Server_Monitor is new Commands.Root_Command with record
      Label    : Ada.Strings.Unbounded.Unbounded_String;
      Language : Ada.Strings.Unbounded.Unbounded_String;
      Request  : GPS.LSP_Client.Requests.Reference;
   end record;

   type Language_Server_Monitor_Access is access all
     Language_Server_Monitor'Class;

   overriding function Name
     (Command : access Language_Server_Monitor) return String;
   overriding function Execute
     (Command : access Language_Server_Monitor)
      return Commands.Command_Return_Type;
   overriding procedure Interrupt (Self : in out Language_Server_Monitor);

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Language_Server_Monitor)
      return Commands.Command_Return_Type is
   begin
      if Command.Request.Has_Request then
         return Commands.Execute_Again;

      else
         return Commands.Success;
      end if;
   end Execute;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Self : in out Language_Server_Monitor) is
   begin
      Self.Request.Cancel;
   end Interrupt;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Language_Server_Monitor) return String is
   begin
      return "[" & Ada.Strings.Unbounded.To_String (Command.Language) & "] "
        & Ada.Strings.Unbounded.To_String (Command.Label);
   end Name;

   ----------------------------------
   -- New_Task_Manager_Integration --
   ----------------------------------

   function New_Task_Manager_Integration
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String) return not null Task_Manager_Integration_Access is
   begin
      return new Task_Manager_Integration'
        (Kernel   => Kernel,
         Language => Ada.Strings.Unbounded.To_Unbounded_String (Language));
   end New_Task_Manager_Integration;

   ---------------------
   -- On_Send_Request --
   ---------------------

   overriding procedure On_Send_Request
     (Self    : in out Task_Manager_Integration;
      Request : GPS.LSP_Client.Requests.Reference)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Command : Language_Server_Monitor_Access;

   begin
      --  Launch a background command to show progress in the Task Manager

      Command := new Language_Server_Monitor'
        (Commands.Root_Command with
           Label    => Ada.Strings.Unbounded.To_Unbounded_String
             (Request.Request.Get_Task_Label),
         Language => Self.Language,
         Request  => Request);

      GPS.Kernel.Task_Manager.Launch_Background_Command
        (Kernel            => Self.Kernel,
         Command           => Command,
         Active            => False,
         Show_Bar          =>
           Command.Label /= Ada.Strings.Unbounded.Null_Unbounded_String,
         Queue_Id          => "language_server",
         Block_Exit        => False,
         Start_Immediately => False);
   end On_Send_Request;

end GPS.LSP_Client.Tasks;
