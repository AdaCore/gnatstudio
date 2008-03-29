-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.Modules;   use GPS.Kernel.Modules;
with String_List_Utils;    use String_List_Utils;
with VCS;                  use VCS;
with Commands.Interactive; use Commands.Interactive;
with VCS_Activities;       use VCS_Activities;

package Commands.VCS is

   type Log_Action_Command_Type is new Root_Command with private;
   type Log_Action_Command_Access is access all Log_Action_Command_Type;

   type Get_Status_Command_Type is new Root_Command with private;
   type Get_Status_Command_Access is access all Get_Status_Command_Type;

   type Update_Files_Command_Type is new Root_Command with private;
   type Update_Files_Command_Access is access all Update_Files_Command_Type;

   type Generic_Kernel_Command is new Interactive_Command with record
      Kernel   : Kernel_Handle;
      Callback : Context_Callback.Marshallers.Void_Marshaller.Handler;
   end record;
   type Generic_Kernel_Command_Access is access all Generic_Kernel_Command;

   --  Activity committed action (check if activity can be closed)

   type Check_Activity_Command_Type is new Root_Command with private;
   type Check_Activity_Command_Access is access Check_Activity_Command_Type;

   procedure Create
     (Item      : out Generic_Kernel_Command_Access;
      Kernel    : Kernel_Handle;
      Callback  : Context_Callback.Marshallers.Void_Marshaller.Handler);
   --  Create a new Generic_Kernel_Command

   procedure Create
     (Item      : out Log_Action_Command_Access;
      Kernel    : Kernel_Handle;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : String_List.List;
      Logs      : String_List.List);
   --  Create a new Log_Action_Command.
   --  The user must free Filenames and Logs after calling Create.
   --  The log files for files that are up-to-date will be erased
   --  after this command completes.

   procedure Create
     (Item      : out Get_Status_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List);
   --  Create a new Get_Status_Command.
   --  The user must free Filenames after calling Create.

   procedure Create
     (Item      : out Update_Files_Command_Access;
      Kernel    : Kernel_Handle;
      Filenames : String_List.List);
   --  Create a new Update_Files_Command.
   --  The user must free Filenames after calling Create.

   procedure Create
     (Item     : out Check_Activity_Command_Access;
      Kernel   : Kernel_Handle;
      Activity : Activity_Id);
   --  Create a new Check_Activity_Command

   function Execute
     (Command : access Log_Action_Command_Type) return Command_Return_Type;
   function Execute
     (Command : access Get_Status_Command_Type) return Command_Return_Type;
   function Execute
     (Command : access Update_Files_Command_Type) return Command_Return_Type;
   function Execute
     (Command : access Generic_Kernel_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   function Execute
     (Command : access Check_Activity_Command_Type)
      return Command_Return_Type;

   function Name (X : access Log_Action_Command_Type) return String;
   function Name (X : access Get_Status_Command_Type) return String;
   function Name (X : access Update_Files_Command_Type) return String;
   function Name (X : access Generic_Kernel_Command) return String;
   function Name (X : access Check_Activity_Command_Type) return String;

   procedure Free (X : in out Log_Action_Command_Type);
   procedure Free (X : in out Get_Status_Command_Type);
   procedure Free (X : in out Update_Files_Command_Type);
   procedure Free (X : in out Generic_Kernel_Command);
   --  Free memory associated to X

private
   type Get_Status_Command_Type is new Root_Command with record
      Rep       : VCS_Access;
      Filenames : String_List.List;
   end record;

   type Update_Files_Command_Type is new Root_Command with record
      Kernel    : Kernel_Handle;
      Filenames : String_List.List;
   end record;

   type Log_Action_Command_Type is new Root_Command with record
      Kernel    : Kernel_Handle;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : String_List.List;
      Logs      : String_List.List;
   end record;

   type Check_Activity_Command_Type is new Root_Command with record
      Kernel   : Kernel_Handle;
      Activity : Activity_Id;
   end record;

end Commands.VCS;
