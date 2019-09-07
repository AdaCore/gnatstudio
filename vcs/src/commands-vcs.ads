------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with String_List_Utils;     use String_List_Utils;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with VCS;                   use VCS;
with Commands.Interactive;  use Commands.Interactive;
with VCS_Activities;        use VCS_Activities;

package Commands.VCS is

   type Log_Action_Command_Type is new Root_Command with private;
   type Log_Action_Command_Access is access all Log_Action_Command_Type;

   type Get_Status_Command_Type is new Root_Command with private;
   type Get_Status_Command_Access is access all Get_Status_Command_Type;

   type Update_Files_Command_Type is new Root_Command with private;
   type Update_Files_Command_Access is access all Update_Files_Command_Type;

   type Generic_Kernel_Command is new Interactive_Command with private;
   type Generic_Kernel_Command_Access is access all Generic_Kernel_Command;

   --  Activity committed action (check if activity can be closed)

   type Check_Activity_Command_Type is new Root_Command with private;
   type Check_Activity_Command_Access is access Check_Activity_Command_Type;

   procedure Create
     (Item     : out Generic_Kernel_Command_Access;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Callback : Context_Callback.Marshallers.Void_Marshaller.Handler);
   --  Create a new Generic_Kernel_Command

   procedure Create
     (Item      : out Log_Action_Command_Access;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : GNATCOLL.VFS.File_Array;
      Logs      : String_List.Vector);
   --  Create a new Log_Action_Command.
   --  The user must free Filenames and Logs after calling Create.
   --  The log files for files that are up-to-date will be erased
   --  after this command completes.

   procedure Create
     (Item      : out Get_Status_Command_Access;
      Rep       : VCS_Access;
      Filenames : GNATCOLL.VFS.File_Array);
   --  Create a new Get_Status_Command.
   --  The user must free Filenames after calling Create.

   procedure Create
     (Item      : out Update_Files_Command_Access;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Filenames : GNATCOLL.VFS.File_Array);
   --  Create a new Update_Files_Command.
   --  The user must free Filenames after calling Create.

   procedure Create
     (Item     : out Check_Activity_Command_Access;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Create a new Check_Activity_Command

   overriding function Execute
     (Command : access Log_Action_Command_Type) return Command_Return_Type;
   overriding function Execute
     (Command : access Get_Status_Command_Type) return Command_Return_Type;
   overriding function Execute
     (Command : access Update_Files_Command_Type) return Command_Return_Type;
   overriding function Execute
     (Command : access Generic_Kernel_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   overriding function Execute
     (Command : access Check_Activity_Command_Type)
      return Command_Return_Type;

   overriding function Name (X : access Log_Action_Command_Type) return String;
   overriding function Name (X : access Get_Status_Command_Type) return String;
   overriding function Name
     (X : access Update_Files_Command_Type) return String;
   overriding function Name (X : access Generic_Kernel_Command) return String;
   overriding function Name
     (X : access Check_Activity_Command_Type) return String;

   overriding procedure Primitive_Free (X : in out Log_Action_Command_Type);
   overriding procedure Primitive_Free (X : in out Get_Status_Command_Type);
   overriding procedure Primitive_Free (X : in out Update_Files_Command_Type);
   --  Free memory associated to X

private
   type Get_Status_Command_Type is new Root_Command with record
      Rep       : VCS_Access;
      Filenames : GNATCOLL.VFS.File_Array_Access;
   end record;

   type Update_Files_Command_Type is new Root_Command with record
      Kernel    : access Kernel_Handle_Record'Class;
      Filenames : GNATCOLL.VFS.File_Array_Access;
   end record;

   type Log_Action_Command_Type is new Root_Command with record
      Kernel    : access Kernel_Handle_Record'Class;
      Rep       : VCS_Access;
      Action    : VCS_Action;
      Filenames : GNATCOLL.VFS.File_Array_Access;
      Logs      : String_List.Vector;
   end record;

   type Check_Activity_Command_Type is new Root_Command with record
      Kernel   : access Kernel_Handle_Record'Class;
      Activity : Activity_Id;
   end record;

   type Generic_Kernel_Command is new Interactive_Command with record
      Kernel   : access Kernel_Handle_Record'Class;
      Callback : Context_Callback.Marshallers.Void_Marshaller.Handler;
   end record;

end Commands.VCS;
